||| `Offler.Audio` on SDL3: an audio stream and the small mixer in
||| `csrc/offler_audio.c`. Independent of the renderer's context -- audio
||| has its own SDL subsystem -- so `initAudio` works with or without a
||| window, and degrades to silent no-ops when there is no output device
||| (headless runs stay headless).
module Offler.Native.Audio

import public Offler.Audio

%default covering

%foreign "C:offler_audio_init,liboffler"
prim__init : PrimIO AnyPtr

%foreign "C:offler_audio_load_pcm,liboffler"
prim__loadPcm : AnyPtr -> Int -> AnyPtr -> Int -> PrimIO Int

%foreign "C:offler_audio_load_wav,liboffler"
prim__loadWav : AnyPtr -> String -> PrimIO Int

%foreign "C:offler_audio_play,liboffler"
prim__play : AnyPtr -> Int -> Double -> Double -> Double -> Int -> PrimIO Int

%foreign "C:offler_audio_voice_gen,liboffler"
prim__voiceGen : AnyPtr -> Int -> PrimIO Int

%foreign "C:offler_audio_set_voice,liboffler"
prim__setVoice : AnyPtr -> Int -> Int -> Double -> Double -> PrimIO ()

%foreign "C:offler_audio_stop_voice,liboffler"
prim__stopVoice : AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_audio_set_master,liboffler"
prim__setMaster : AnyPtr -> Double -> PrimIO ()

public export
record NativeAudio where
  constructor MkNativeAudio
  ctx : AnyPtr

export
initAudio : IO NativeAudio
initAudio = MkNativeAudio <$> primIO prim__init

export
Audio NativeAudio where
  loadSound a (SoundPcm rate xs) = do
    (p, n) <- stagePcm xs
    soundHandle <$> primIO (prim__loadPcm a.ctx rate p n)
  loadSound a (SoundFile path) =
    soundHandle <$> primIO (prim__loadWav a.ctx path)

  play a h pb = do
    vi <- primIO (prim__play a.ctx (soundIndex h)
                    pb.gain pb.pan pb.rate (if pb.loop then 1 else 0))
    if vi < 0
      then pure noVoice
      else voice vi <$> primIO (prim__voiceGen a.ctx vi)

  setVoice a v g p = primIO (prim__setVoice a.ctx (voiceIndex v) (voiceGen v) g p)

  stopVoice a v = primIO (prim__stopVoice a.ctx (voiceIndex v) (voiceGen v))

  setMasterGain a g = primIO (prim__setMaster a.ctx g)
