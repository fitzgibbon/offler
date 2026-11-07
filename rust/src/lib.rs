//! offler's native layer for the Idris 2 Rust backend: every
//! `%foreign "Rust:offler_*"` lands on a function of the same name here.
//! It replaces the C shim in `csrc/` function for function: winit for the
//! window and input, wgpu for drawing, gilrs for gamepads, rodio for sound
//! and `image` for texture decoding.
//!
//! Handles are shared by Idris, so every handle type mutates through
//! interior mutability; nothing here is `Send`, as the program is one
//! thread.

#![forbid(unsafe_code)]

mod array;
mod audio;
mod gamepad;
mod input;
mod keys;
mod native;
mod render;
mod spec;

pub use array::*;
pub use audio::{
    AudioCtx, offler_audio_init, offler_audio_load_pcm, offler_audio_load_wav, offler_audio_play,
    offler_audio_set_master, offler_audio_set_voice, offler_audio_stop_voice,
    offler_audio_voice_gen,
};
pub use native::*;
