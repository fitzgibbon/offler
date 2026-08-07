||| Sound, abstracted over browser and desktop: Web Audio on one side, an
||| SDL3 mixer on the other, one vocabulary over both -- the same bargain as
||| `Offler.Gfx.Renderer`, at the same granularity. Sounds are *retained*
||| (loaded once, played many times); a play mints a `Voice`, which is a
||| generational handle exactly like a `MeshHandle`: a finished voice's slot
||| is recycled and its generation bumped, so a stale `Voice` controls
||| nothing rather than someone else's sound.
module Offler.Audio

import Offler.Gfx.Array

%default covering

||| Where a sound comes from. `SoundPcm` is mono samples in -1..1 at a given
||| rate -- the procedural path, and the only one that needs no files at
||| all. `SoundFile` is a path (desktop) or URL (browser); the browser
||| decodes whatever it decodes for `<audio>`, the desktop decodes WAV.
public export
data SoundSource : Type where
  SoundPcm : (sampleRate : Int) -> (samples : List Double) -> SoundSource
  SoundFile : (path : String) -> SoundSource

||| A loaded sound. Sounds are permanent, like textures: load at startup,
||| play forever.
export
data SoundHandle : Type where
  MkSoundHandle : Int -> SoundHandle

||| For backends only.
export %inline
soundHandle : Int -> SoundHandle
soundHandle = MkSoundHandle

||| For backends only.
export %inline
soundIndex : SoundHandle -> Int
soundIndex (MkSoundHandle i) = i

||| One playing (or finished) sound: an index and the generation it was
||| minted at. The constructor is private; backends mint them, and a voice
||| whose generation has moved on is simply inert.
export
data Voice : Type where
  MkVoice : (idx : Int) -> (gen : Int) -> Voice

||| For backends only.
export %inline
voice : (idx : Int) -> (gen : Int) -> Voice
voice = MkVoice

||| For backends only.
export %inline
voiceIndex : Voice -> Int
voiceIndex (MkVoice i _) = i

||| For backends only.
export %inline
voiceGen : Voice -> Int
voiceGen (MkVoice _ g) = g

||| The voice that never played: what `play` returns when the sound is not
||| ready (a URL still decoding) or the device never opened. Controlling it
||| does nothing, which is the point.
export
noVoice : Voice
noVoice = MkVoice (-1) 0

||| How to play a sound: linear gain (1 nominal), pan (-1 left .. 1 right),
||| rate (1 nominal; pitch scales with speed, tape-style), loop.
public export
record Playback where
  constructor MkPlayback
  gain : Double
  pan : Double
  rate : Double
  loop : Bool

||| Once, centred, full gain, natural pitch.
public export
playback : Playback
playback = MkPlayback 1.0 0.0 1.0 False

public export
withGain : Double -> Playback -> Playback
withGain g = { gain := g }

public export
panned : Double -> Playback -> Playback
panned x = { pan := x }

public export
atRate : Double -> Playback -> Playback
atRate r = { rate := r }

||| Pitch as an interval: `semitones 12` plays an octave up, at the cost of
||| the corresponding speed change.
public export
semitones : Double -> Playback -> Playback
semitones n = { rate := pow 2.0 (n / 12.0) }

public export
looping : Playback -> Playback
looping = { loop := True }

||| The unified audio interface. Two implementations: `Offler.Web.Audio`
||| over Web Audio, `Offler.Native.Audio` over an SDL3 stream and a small
||| mixer. Browsers refuse to make sound before a user gesture; the web
||| backend resumes itself on the first key or pointer, so early plays are
||| silently dropped rather than errors.
public export
interface Audio a where
  ||| Load (or begin loading) a sound. Decoding a URL is asynchronous in the
  ||| browser; the handle is valid immediately and plays silence until the
  ||| decode finishes -- the renderer's handle-before-loaded bargain.
  loadSound : a -> SoundSource -> IO SoundHandle

  ||| Start the sound, minting the voice that controls it. A one-shot
  ||| voice's slot is recycled when it ends.
  play : a -> SoundHandle -> Playback -> IO Voice

  ||| Adjust a live voice. Stale voices (ended, or stopped and recycled)
  ||| no-op -- one integer compare, the `freeMesh` trick.
  setVoice : a -> Voice -> (gain : Double) -> (pan : Double) -> IO ()

  ||| Stop a voice now. Idempotent; stale voices no-op.
  stopVoice : a -> Voice -> IO ()

  ||| Scale everything at once, 1 nominal.
  setMasterGain : a -> Double -> IO ()

--------------------------------------------------------------------------------
-- Staging, shared by both backends

lengthOf : Int -> List Double -> Int
lengthOf n [] = n
lengthOf n (_ :: rest) = lengthOf (n + 1) rest

fill : F32Array cap -> Int -> List Double -> PrimIO Int
fill a i [] w = MkIORes i w
fill a i (v :: rest) w =
  case toPrim (poke a (unsafeAt i) v) w of
    MkIORes _ w' => fill a (i + 1) rest w'

||| Stage mono samples into a typed array for a backend to hand its FFI:
||| the pointer and the sample count. The loop is the trampolined `PrimIO`
||| self tail call (a second of audio is tens of thousands of elements,
||| which a plain `map` would answer with a stack overflow on JS).
export
stagePcm : List Double -> IO (AnyPtr, Int)
stagePcm xs = do
  let n = lengthOf 0 xs
  a <- newF32 (max 1 n)
  ignore (fromPrim (fill a 0 xs))
  pure (raw a, n)
