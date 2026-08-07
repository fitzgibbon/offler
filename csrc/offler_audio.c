/* The desktop half of Offler.Audio: SDL3's audio stream plus a small
 * mixer. SDL owns the device and the format conversion to it; this file
 * owns voices. Sounds are stored mono f32 *at the device rate* (converted
 * once at load), so the callback's per-sample work is one linear
 * interpolation, a gain, and a constant-power pan.
 *
 * The callback runs on SDL's audio thread. Every function that touches
 * voices or the sounds array holds SDL_LockAudioStream, which serialises
 * with the callback; conversions and file loads happen outside the lock.
 *
 * Voices are generational, like the renderer's meshes: a finished or
 * stopped voice bumps its generation, so a stale Voice handle from Idris
 * compares unequal and no-ops. If the device cannot be opened (headless
 * CI, no sound server) everything degrades to silent no-ops. */

#include <SDL3/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define OFFLER_MAX_VOICES 64

typedef struct {
  float *data;                /* mono f32 at device rate; never freed */
  int len;
} ASound;

typedef struct {
  int sound;
  double pos, rate;
  float gain, pan;
  int loop, live, gen;
} AVoice;

typedef struct {
  SDL_AudioStream *stream;    /* NULL: no device, every call no-ops */
  SDL_AudioSpec spec;         /* our side of the stream: f32 stereo */
  ASound *sounds;
  int soundCount, soundCap;
  AVoice voices[OFFLER_MAX_VOICES];
  float master;
  float *mix;                 /* scratch, frames*2 floats */
  int mixCap;
} AudioCtx;

static void audio_cb(void *ud, SDL_AudioStream *stream, int additional, int total) {
  (void)total;
  AudioCtx *a = (AudioCtx *)ud;
  int frames = additional / (int)(sizeof(float) * 2);
  if (frames <= 0) return;
  if (frames * 2 > a->mixCap) {
    a->mixCap = frames * 2;
    a->mix = (float *)realloc(a->mix, sizeof(float) * (size_t)a->mixCap);
  }
  float *mix = a->mix;
  memset(mix, 0, sizeof(float) * (size_t)frames * 2);

  for (int vi = 0; vi < OFFLER_MAX_VOICES; vi++) {
    AVoice *v = &a->voices[vi];
    if (!v->live) continue;
    ASound *s = &a->sounds[v->sound];
    if (s->len <= 0) { v->live = 0; v->gen++; continue; }
    /* Constant power: full left is (1,0), centre (.707,.707). */
    double ang = ((double)v->pan + 1.0) * 0.25 * M_PI;
    float lg = v->gain * a->master * (float)cos(ang);
    float rg = v->gain * a->master * (float)sin(ang);
    double pos = v->pos;
    for (int f = 0; f < frames; f++) {
      if (pos >= (double)s->len) {
        if (v->loop) { pos = fmod(pos, (double)s->len); }
        else { v->live = 0; v->gen++; break; }
      }
      int i0 = (int)pos;
      float frac = (float)(pos - (double)i0);
      float s0 = s->data[i0];
      float s1 = (i0 + 1 < s->len) ? s->data[i0 + 1]
               : (v->loop ? s->data[0] : s0);
      float smp = s0 + (s1 - s0) * frac;
      mix[2 * f] += smp * lg;
      mix[2 * f + 1] += smp * rg;
      pos += v->rate;
    }
    v->pos = pos;
  }
  SDL_PutAudioStreamData(stream, mix, frames * 2 * (int)sizeof(float));
}

void *offler_audio_init(void) {
  AudioCtx *a = (AudioCtx *)calloc(1, sizeof(AudioCtx));
  a->master = 1.0f;
  if (!SDL_InitSubSystem(SDL_INIT_AUDIO)) {
    fprintf(stderr, "offler audio: SDL audio unavailable: %s\n", SDL_GetError());
    return a;
  }
  a->spec.format = SDL_AUDIO_F32;
  a->spec.channels = 2;
  a->spec.freq = 48000;
  a->stream = SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
                                        &a->spec, audio_cb, a);
  if (!a->stream)
    fprintf(stderr, "offler audio: no output device: %s\n", SDL_GetError());
  else
    SDL_ResumeAudioStreamDevice(a->stream);
  return a;
}

/* Store a sound converted to mono f32 at the device rate. The sounds array
 * itself may move on append, and the callback indexes into it, so the
 * append is under the lock; the conversion is not. */
static int sound_add(AudioCtx *a, float *data, int len) {
  if (a->stream) SDL_LockAudioStream(a->stream);
  if (a->soundCount == a->soundCap) {
    a->soundCap = a->soundCap ? a->soundCap * 2 : 16;
    a->sounds = (ASound *)realloc(a->sounds, sizeof(ASound) * (size_t)a->soundCap);
  }
  a->sounds[a->soundCount].data = data;
  a->sounds[a->soundCount].len = len;
  int id = a->soundCount++;
  if (a->stream) SDL_UnlockAudioStream(a->stream);
  return id;
}

static int sound_convert(AudioCtx *a, const SDL_AudioSpec *src,
                         const Uint8 *bytes, int byteLen) {
  SDL_AudioSpec dst = { SDL_AUDIO_F32, 1, a->spec.freq };
  Uint8 *out = NULL;
  int outLen = 0;
  if (!SDL_ConvertAudioSamples(src, bytes, byteLen, &dst, &out, &outLen)) {
    fprintf(stderr, "offler audio: convert: %s\n", SDL_GetError());
    return sound_add(a, NULL, 0);
  }
  return sound_add(a, (float *)out, outLen / (int)sizeof(float));
}

int offler_audio_load_pcm(void *p, int rate, void *data, int n) {
  AudioCtx *a = (AudioCtx *)p;
  SDL_AudioSpec src = { SDL_AUDIO_F32, 1, rate > 0 ? rate : 48000 };
  return sound_convert(a, &src, (const Uint8 *)data, n * (int)sizeof(float));
}

int offler_audio_load_wav(void *p, const char *path) {
  AudioCtx *a = (AudioCtx *)p;
  SDL_AudioSpec spec;
  Uint8 *buf = NULL;
  Uint32 len = 0;
  if (!SDL_LoadWAV(path, &spec, &buf, &len)) {
    fprintf(stderr, "offler audio: %s: %s\n", path, SDL_GetError());
    return sound_add(a, NULL, 0);
  }
  int id = sound_convert(a, &spec, buf, (int)len);
  SDL_free(buf);
  return id;
}

int offler_audio_play(void *p, int sid, double gain, double pan,
                      double rate, int loop) {
  AudioCtx *a = (AudioCtx *)p;
  if (!a->stream || sid < 0 || sid >= a->soundCount) return -1;
  SDL_LockAudioStream(a->stream);
  int vi = -1;
  for (int i = 0; i < OFFLER_MAX_VOICES; i++)
    if (!a->voices[i].live) { vi = i; break; }
  if (vi >= 0) {
    AVoice *v = &a->voices[vi];
    v->sound = sid;
    v->pos = 0.0;
    v->rate = rate > 0.0 ? rate : 1.0;
    v->gain = (float)gain;
    v->pan = (float)(pan < -1.0 ? -1.0 : pan > 1.0 ? 1.0 : pan);
    v->loop = loop;
    v->live = 1;
  }
  SDL_UnlockAudioStream(a->stream);
  return vi;
}

int offler_audio_voice_gen(void *p, int vi) {
  AudioCtx *a = (AudioCtx *)p;
  if (vi < 0 || vi >= OFFLER_MAX_VOICES) return 0;
  if (a->stream) SDL_LockAudioStream(a->stream);
  int g = a->voices[vi].gen;
  if (a->stream) SDL_UnlockAudioStream(a->stream);
  return g;
}

void offler_audio_set_voice(void *p, int vi, int gen, double gain, double pan) {
  AudioCtx *a = (AudioCtx *)p;
  if (!a->stream || vi < 0 || vi >= OFFLER_MAX_VOICES) return;
  SDL_LockAudioStream(a->stream);
  AVoice *v = &a->voices[vi];
  if (v->live && v->gen == gen) {
    v->gain = (float)gain;
    v->pan = (float)(pan < -1.0 ? -1.0 : pan > 1.0 ? 1.0 : pan);
  }
  SDL_UnlockAudioStream(a->stream);
}

void offler_audio_stop_voice(void *p, int vi, int gen) {
  AudioCtx *a = (AudioCtx *)p;
  if (!a->stream || vi < 0 || vi >= OFFLER_MAX_VOICES) return;
  SDL_LockAudioStream(a->stream);
  AVoice *v = &a->voices[vi];
  if (v->live && v->gen == gen) { v->live = 0; v->gen++; }
  SDL_UnlockAudioStream(a->stream);
}

void offler_audio_set_master(void *p, double g) {
  AudioCtx *a = (AudioCtx *)p;
  if (!a->stream) return;
  SDL_LockAudioStream(a->stream);
  a->master = (float)g;
  SDL_UnlockAudioStream(a->stream);
}

void offler_audio_quit(void *p) {
  AudioCtx *a = (AudioCtx *)p;
  if (!a) return;
  if (a->stream) SDL_DestroyAudioStream(a->stream);
  for (int i = 0; i < a->soundCount; i++) SDL_free(a->sounds[i].data);
  free(a->sounds);
  free(a->mix);
  free(a);
}
