//! The desktop half of `Offler.Audio`: rodio's output device plus the
//! C shim's small mixer, ported verbatim. Sounds are stored mono f32 at the
//! mixer rate (converted once at load), so the per-sample work is one
//! linear interpolation, a gain and a constant-power pan.
//!
//! The mixer runs on rodio's audio thread; every function touching voices
//! or the sound list holds the mixer's mutex, as the C held
//! `SDL_LockAudioStream`. Voices are generational: a finished or stopped
//! voice bumps its generation so a stale handle no-ops. Without an output
//! device everything degrades to silent no-ops.

use std::f64::consts::PI;
use std::num::NonZero;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::Duration;

use rodio::{MixerDeviceSink, Source};

use crate::array::{F32Array, count};

pub const MAX_VOICES: usize = 64;
/// The rate the C asked SDL for, which sounds are converted to.
pub const MIX_RATE: u32 = 48_000;
/// Frames mixed per lock taken by the audio thread.
const BLOCK_FRAMES: usize = 256;

#[derive(Clone, Copy, Default)]
struct Voice {
    sound: usize,
    pos: f64,
    rate: f64,
    gain: f32,
    pan: f32,
    looping: bool,
    live: bool,
    generation: i32,
}

pub struct Mix {
    sounds: Vec<Arc<[f32]>>,
    voices: [Voice; MAX_VOICES],
    master: f32,
}

impl Default for Mix {
    fn default() -> Self {
        Self {
            sounds: Vec::new(),
            voices: [Voice::default(); MAX_VOICES],
            master: 1.0,
        }
    }
}

impl Mix {
    /// The C's `audio_cb`: overwrite `out` (interleaved stereo) with the
    /// mix of every live voice.
    pub fn render(&mut self, out: &mut [f32]) {
        out.fill(0.0);
        let frames = out.len() / 2;
        let master = self.master;
        for v in self.voices.iter_mut().filter(|v| v.live) {
            let s = &self.sounds[v.sound];
            let len = s.len();
            if len == 0 {
                v.live = false;
                v.generation = v.generation.wrapping_add(1);
                continue;
            }
            // Constant power: full left is (1,0), centre (.707,.707).
            let ang = (f64::from(v.pan) + 1.0) * 0.25 * PI;
            let lg = v.gain * master * ang.cos() as f32;
            let rg = v.gain * master * ang.sin() as f32;
            let mut pos = v.pos;
            for f in 0..frames {
                if pos >= len as f64 {
                    if v.looping {
                        pos %= len as f64;
                    } else {
                        v.live = false;
                        v.generation = v.generation.wrapping_add(1);
                        break;
                    }
                }
                let i0 = pos as usize;
                let frac = (pos - i0 as f64) as f32;
                let s0 = s[i0];
                let s1 = if i0 + 1 < len {
                    s[i0 + 1]
                } else if v.looping {
                    s[0]
                } else {
                    s0
                };
                let smp = s0 + (s1 - s0) * frac;
                out[2 * f] += smp * lg;
                out[2 * f + 1] += smp * rg;
                pos += v.rate;
            }
            v.pos = pos;
        }
    }

    fn add(&mut self, data: Vec<f32>) -> i64 {
        self.sounds.push(data.into());
        self.sounds.len() as i64 - 1
    }

    fn play(&mut self, sid: usize, gain: f64, pan: f64, rate: f64, looping: bool) -> i64 {
        let Some(vi) = self.voices.iter().position(|v| !v.live) else {
            return -1;
        };
        let v = &mut self.voices[vi];
        v.sound = sid;
        v.pos = 0.0;
        v.rate = if rate > 0.0 { rate } else { 1.0 };
        v.gain = gain as f32;
        v.pan = clamp_pan(pan);
        v.looping = looping;
        v.live = true;
        vi as i64
    }

    fn live_voice(&mut self, vi: i64, generation: i64) -> Option<&mut Voice> {
        let v = self.voices.get_mut(voice_index(vi)?)?;
        (v.live && i64::from(v.generation) == i64::from(generation as i32)).then_some(v)
    }
}

fn clamp_pan(pan: f64) -> f32 {
    pan.clamp(-1.0, 1.0) as f32
}

fn voice_index(vi: i64) -> Option<usize> {
    usize::try_from(vi).ok().filter(|&i| i < MAX_VOICES)
}

/// Mono, linearly resampled from `from` Hz to `to` Hz.
pub fn resample(src: &[f32], from: u32, to: u32) -> Vec<f32> {
    if src.is_empty() || from == 0 || to == 0 {
        return Vec::new();
    }
    if from == to {
        return src.to_vec();
    }
    let step = f64::from(from) / f64::from(to);
    let n = (src.len() as f64 / step).round() as usize;
    let last = src.len() - 1;
    (0..n)
        .map(|i| {
            let t = i as f64 * step;
            let i0 = (t as usize).min(last);
            let frac = (t - i0 as f64) as f32;
            let s0 = src[i0];
            let s1 = src[(i0 + 1).min(last)];
            s0 + (s1 - s0) * frac
        })
        .collect()
}

/// Interleaved frames of `channels` down to mono, by averaging.
pub fn downmix(samples: &[f32], channels: usize) -> Vec<f32> {
    if channels <= 1 {
        return samples.to_vec();
    }
    samples
        .chunks_exact(channels)
        .map(|fr| fr.iter().sum::<f32>() / channels as f32)
        .collect()
}

/// The mixer as a rodio source: stereo at the mixer rate, endless.
struct MixSource {
    mix: Arc<Mutex<Mix>>,
    buf: Vec<f32>,
    at: usize,
}

impl Iterator for MixSource {
    type Item = rodio::Sample;

    fn next(&mut self) -> Option<rodio::Sample> {
        if self.at == self.buf.len() {
            lock(&self.mix).render(&mut self.buf);
            self.at = 0;
        }
        let s = self.buf[self.at];
        self.at += 1;
        Some(s)
    }
}

impl Source for MixSource {
    fn current_span_len(&self) -> Option<usize> {
        None
    }

    fn channels(&self) -> rodio::ChannelCount {
        NonZero::new(2).expect("two channels")
    }

    fn sample_rate(&self) -> rodio::SampleRate {
        NonZero::new(MIX_RATE).expect("nonzero rate")
    }

    fn total_duration(&self) -> Option<Duration> {
        None
    }
}

fn lock(m: &Mutex<Mix>) -> MutexGuard<'_, Mix> {
    m.lock().unwrap_or_else(|e| e.into_inner())
}

pub struct AudioCtx {
    mix: Arc<Mutex<Mix>>,
    /// `None`: no device, and playback calls no-op.
    sink: Option<MixerDeviceSink>,
}

impl AudioCtx {
    fn store(&self, data: Vec<f32>) -> i64 {
        lock(&self.mix).add(data)
    }

    fn convert(&self, mono: &[f32], rate: u32) -> i64 {
        self.store(resample(mono, rate, MIX_RATE))
    }
}

pub fn offler_audio_init() -> AudioCtx {
    let mix = Arc::new(Mutex::new(Mix::default()));
    let sink = match rodio::DeviceSinkBuilder::open_default_sink() {
        Ok(mut s) => {
            s.log_on_drop(false);
            s.mixer().add(MixSource {
                mix: mix.clone(),
                buf: vec![0.0; BLOCK_FRAMES * 2],
                at: BLOCK_FRAMES * 2,
            });
            Some(s)
        }
        Err(e) => {
            eprintln!("offler audio: no output device: {e}");
            None
        }
    };
    AudioCtx { mix, sink }
}

pub fn offler_audio_load_pcm(a: &AudioCtx, rate: i64, data: &F32Array, n: i64) -> i64 {
    let rate = u32::try_from(rate)
        .ok()
        .filter(|&r| r > 0)
        .unwrap_or(MIX_RATE);
    let src = data.prefix(count(n));
    a.convert(&src, rate)
}

pub fn offler_audio_load_wav(a: &AudioCtx, path: &str) -> i64 {
    match decode_file(path) {
        Ok((mono, rate)) => a.convert(&mono, rate),
        Err(e) => {
            eprintln!("offler audio: {path}: {e}");
            a.store(Vec::new())
        }
    }
}

fn decode_file(path: &str) -> Result<(Vec<f32>, u32), Box<dyn std::error::Error>> {
    let dec = rodio::Decoder::try_from(std::fs::File::open(path)?)?;
    let channels = usize::from(dec.channels().get());
    let rate = dec.sample_rate().get();
    let samples: Vec<f32> = dec.collect();
    Ok((downmix(&samples, channels), rate))
}

pub fn offler_audio_play(
    a: &AudioCtx,
    sid: i64,
    gain: f64,
    pan: f64,
    rate: f64,
    looping: i64,
) -> i64 {
    if a.sink.is_none() {
        return -1;
    }
    let mut m = lock(&a.mix);
    match usize::try_from(sid).ok().filter(|&s| s < m.sounds.len()) {
        Some(s) => m.play(s, gain, pan, rate, looping as i32 != 0),
        None => -1,
    }
}

pub fn offler_audio_voice_gen(a: &AudioCtx, vi: i64) -> i64 {
    voice_index(vi).map_or(0, |i| i64::from(lock(&a.mix).voices[i].generation))
}

pub fn offler_audio_set_voice(a: &AudioCtx, vi: i64, generation: i64, gain: f64, pan: f64) {
    if a.sink.is_none() {
        return;
    }
    if let Some(v) = lock(&a.mix).live_voice(vi, generation) {
        v.gain = gain as f32;
        v.pan = clamp_pan(pan);
    }
}

pub fn offler_audio_stop_voice(a: &AudioCtx, vi: i64, generation: i64) {
    if a.sink.is_none() {
        return;
    }
    if let Some(v) = lock(&a.mix).live_voice(vi, generation) {
        v.live = false;
        v.generation = v.generation.wrapping_add(1);
    }
}

pub fn offler_audio_set_master(a: &AudioCtx, g: f64) {
    if a.sink.is_some() {
        lock(&a.mix).master = g as f32;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mix_with(sound: Vec<f32>) -> Mix {
        let mut m = Mix::default();
        m.add(sound);
        m
    }

    #[test]
    fn voices_take_the_first_free_slot_and_run_out() {
        let mut m = mix_with(vec![1.0; 4]);
        for k in 0..MAX_VOICES {
            assert_eq!(m.play(0, 1.0, 0.0, 1.0, false), k as i64);
        }
        assert_eq!(m.play(0, 1.0, 0.0, 1.0, false), -1);
    }

    #[test]
    fn a_finished_voice_bumps_its_generation_and_frees_its_slot() {
        let mut m = mix_with(vec![1.0; 3]);
        let v = m.play(0, 1.0, 0.0, 1.0, false);
        assert_eq!(m.voices[0].generation, 0);
        let mut out = vec![0.0; 16];
        m.render(&mut out);
        assert!(!m.voices[0].live);
        assert_eq!(m.voices[0].generation, 1);
        assert!(m.live_voice(v, 0).is_none());
        assert_eq!(m.play(0, 1.0, 0.0, 1.0, false), 0);
        assert!(m.live_voice(0, 1).is_some());
    }

    #[test]
    fn stale_generations_and_bad_indices_are_ignored() {
        let mut m = mix_with(vec![1.0; 100]);
        let v = m.play(0, 1.0, 0.0, 1.0, false);
        assert!(m.live_voice(v, 1).is_none());
        assert!(m.live_voice(-1, 0).is_none());
        assert!(m.live_voice(MAX_VOICES as i64, 0).is_none());
        assert!(m.live_voice(v, 0).is_some());
    }

    #[test]
    fn centre_pan_is_constant_power_and_master_scales() {
        let mut m = mix_with(vec![1.0; 100]);
        m.master = 0.5;
        m.play(0, 1.0, 0.0, 1.0, false);
        let mut out = vec![0.0; 4];
        m.render(&mut out);
        let h = 0.5 * std::f32::consts::FRAC_1_SQRT_2;
        assert!((out[0] - h).abs() < 1e-6 && (out[1] - h).abs() < 1e-6);
    }

    #[test]
    fn hard_left_is_left_only_and_pan_clamps() {
        let mut m = mix_with(vec![1.0; 100]);
        m.play(0, 1.0, -5.0, 1.0, false);
        let mut out = vec![0.0; 2];
        m.render(&mut out);
        assert!((out[0] - 1.0).abs() < 1e-6);
        assert!(out[1].abs() < 1e-6);
    }

    #[test]
    fn interpolates_between_samples_and_loops_back() {
        let mut m = mix_with(vec![0.0, 1.0]);
        m.play(0, 1.0, -1.0, 0.5, true);
        let mut out = vec![0.0; 10];
        m.render(&mut out);
        let left: Vec<f32> = out.iter().step_by(2).copied().collect();
        assert_eq!(left, vec![0.0, 0.5, 1.0, 0.5, 0.0]);
        assert!(m.voices[0].live);
    }

    #[test]
    fn empty_sounds_end_their_voice_at_once() {
        let mut m = mix_with(Vec::new());
        m.play(0, 1.0, 0.0, 1.0, true);
        let mut out = vec![0.0; 4];
        m.render(&mut out);
        assert!(!m.voices[0].live);
        assert_eq!(m.voices[0].generation, 1);
        assert_eq!(out, vec![0.0; 4]);
    }

    #[test]
    fn non_positive_rates_play_at_unit_rate() {
        let mut m = mix_with(vec![1.0; 4]);
        m.play(0, 1.0, 0.0, -2.0, false);
        assert_eq!(m.voices[0].rate, 1.0);
    }

    #[test]
    fn resample_and_downmix() {
        assert_eq!(resample(&[1.0, 2.0], 48_000, 48_000), vec![1.0, 2.0]);
        assert_eq!(
            resample(&[0.0, 1.0], 24_000, 48_000),
            vec![0.0, 0.5, 1.0, 1.0]
        );
        assert_eq!(
            resample(&[0.0, 1.0, 2.0, 3.0], 96_000, 48_000),
            vec![0.0, 2.0]
        );
        assert!(resample(&[], 1, 2).is_empty());
        assert_eq!(downmix(&[1.0, 3.0, 0.0, 2.0], 2), vec![2.0, 1.0]);
    }

    #[test]
    fn a_context_without_a_device_still_stores_sounds() {
        let a = AudioCtx {
            mix: Arc::new(Mutex::new(Mix::default())),
            sink: None,
        };
        let pcm = crate::array::offler_f32_new(4);
        assert_eq!(offler_audio_load_pcm(&a, 0, &pcm, 4), 0);
        assert_eq!(offler_audio_load_wav(&a, "/nonexistent.wav"), 1);
        assert_eq!(offler_audio_play(&a, 0, 1.0, 0.0, 1.0, 0), -1);
        assert_eq!(offler_audio_voice_gen(&a, 0), 0);
        assert_eq!(offler_audio_voice_gen(&a, 99), 0);
        offler_audio_set_master(&a, 0.0);
        assert_eq!(lock(&a.mix).master, 1.0);
    }
}
