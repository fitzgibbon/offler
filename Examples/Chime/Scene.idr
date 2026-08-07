||| Fourteen chime bars and every input offler understands: click or tap a
||| bar (pointer and touch are separate vocabularies), walk the highlight
||| with arrows, a gamepad stick or -- under pointer lock -- raw pointer
||| deltas, and strike with Space, Enter or the pad's South button. Each
||| strike plays one *procedural* sound (a plucked sine, synthesised at
||| startup, no asset files) pitched a pentatonic step per bar and panned
||| to the bar's position; the wheel rides the master gain. The ripples
||| are immediate-mode gizmo circles, so the audio, input and gizmo
||| systems all meet in one scene.
module Examples.Chime.Scene

import Data.IORef
import Examples.Util
import Offler.Audio
import Offler.Camera
import Offler.Color
import Offler.Gizmos
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math
import Offler.Mesh
import Offler.Picking
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

barCount : Int
barCount = 14

barX : Int -> Double
barX i = (cast i - cast (barCount - 1) * 0.5) * 1.15

barHue : Int -> Double
barHue i = cast i / cast barCount

||| A pentatonic step per bar, starting an octave below the sample's pitch.
semisOf : Int -> Double
semisOf i =
  let step = case mod i 5 of
               0 => 0
               1 => 2
               2 => 4
               3 => 7
               _ => 9
   in cast (step + 12 * div i 5) - 12.0

--------------------------------------------------------------------------------
-- The instrument: one plucked sine, synthesised at startup

plinkRate : Int
plinkRate = 22050

||| ~0.8 s: a fundamental with two soft partials under a fast attack and
||| an exponential decay. Built with an accumulator, from the last sample
||| backwards -- tens of thousands of elements, the usual JS-stack rule.
plink : List Double
plink = go (18000 - 1) []
  where
    at : Int -> Double
    at i =
      let t = cast i / cast plinkRate
          env = exp (-6.0 * t) * min (t / 0.004) 1.0
          s = sin (tau * 392.0 * t)
            + 0.4 * sin (tau * 784.0 * t)
            + 0.15 * sin (tau * 1176.0 * t)
       in 0.32 * env * s

    go : Int -> List Double -> List Double
    go i acc = if i < 0 then acc else go (i - 1) (at i :: acc)

--------------------------------------------------------------------------------
-- World

record World where
  constructor MkWorld
  ||| Struck bars: (bar, strike time). Pruned as the ripples fade.
  ripples : IORef (List (Int, Double))
  ||| The highlight, a continuous bar position.
  cursor : IORef Double
  lastT : IORef Double
  southHeld : IORef Bool
  locked : IORef Bool
  cursorShown : IORef Bool
  gain : IORef Double

newWorld : IO World
newWorld =
  [| MkWorld (newIORef []) (newIORef (cast (div barCount 2)))
             (newIORef 0.0) (newIORef False) (newIORef False)
             (newIORef True) (newIORef 1.0) |]

clampD : Double -> Double -> Double -> Double
clampD lo hi v = max lo (min hi v)

camera : Camera
camera = perspectiveCamera
  (lookingAt zero3 (v3 0.0 1.0 0.0) (at (v3 0.0 5.2 11.5)))

lights : Lights
lights = MkLights 0.22 [MkDirectional (v3 (-0.35) (-1.0) (-0.3)) (rgb 1.0 0.96 0.9)]

--------------------------------------------------------------------------------
-- Input

strike : Audio au => au -> SoundHandle -> World -> Double -> Int -> IO ()
strike au snd w t i0 = do
  let i = the Int (cast (clampD 0.0 (cast barCount - 1.0) (cast i0)))
  ignore (play au snd (semitones (semisOf i)
                       (panned (barX i / 8.0) (withGain 0.85 playback))))
  writeIORef w.cursor (cast i)
  modifyIORef w.ripples ((i, t) ::)

adjustGain : Audio au => au -> World -> Status -> Double -> IO ()
adjustGain au w status d = do
  g <- map (clampD 0.0 1.5 . (+ d)) (readIORef w.gain)
  writeIORef w.gain g
  setMasterGain au g
  status "count-label" ("gain " ++ show g)

handle : Renderer r f => Platform p => Audio au =>
         r -> p -> au -> SoundHandle -> World -> Status -> Double
      -> Event -> IO ()
handle r p au snd w status t = go
  where
    moveCursor : Double -> IO ()
    moveCursor d = modifyIORef w.cursor
      (clampD 0.0 (cast barCount - 1.0) . (+ d))

    cursorBar : IO Int
    cursorBar = do
      c <- readIORef w.cursor
      pure (cast c)

    go : Event -> IO ()
    -- Unlocked presses and touches arrive through the picker, which ray-
    -- casts the actual bars. Locked, the pointer has no position, so a
    -- press strikes the highlighted bar -- the South-button mapping.
    go (PointerDown LeftButton _ _) = do
      True <- readIORef w.locked
        | False => pure ()
      strike au snd w t !cursorBar
    go (PointerDelta dx _) = do
      True <- readIORef w.locked
        | False => pure ()
      moveCursor (dx * 0.02)
    go (KeyDown "ArrowLeft") = moveCursor (-1.0)
    go (KeyDown "ArrowRight") = moveCursor 1.0
    go (KeyDown " ") = strike au snd w t !cursorBar
    go (KeyDown "Enter") = strike au snd w t !cursorBar
    -- Only the *request*: the ref follows PointerLockChanged, because the
    -- platform may grant late, decline, or release on its own (Escape).
    go (KeyDown "l") = do
      locked <- readIORef w.locked
      setPointerLock p (not locked)
    go (PointerLockChanged on) = do
      writeIORef w.locked on
      status "note" (if on then "pointer locked: deltas steer, l releases"
                           else "l locks the pointer")
    go (KeyDown "c") = do
      shown <- map not (readIORef w.cursorShown)
      writeIORef w.cursorShown shown
      setCursorVisible p shown
    go (Wheel d) = adjustGain au w status (d * 0.05)
    go (GamepadConnected _ n) = status "stats" n
    go (GamepadDisconnected _) = status "stats" ""
    go _ = pure ()

||| The polled half: the stick steers the highlight, South strikes on its
||| press edge.
pollPads : Platform p => Audio au =>
           p -> au -> SoundHandle -> World -> Double -> Double -> IO ()
pollPads p au snd w t dt = do
  (g :: _) <- gamepads p
    | [] => pure ()
  let ax = if abs g.leftX > 0.15 then g.leftX else 0.0
  modifyIORef w.cursor
    (clampD 0.0 (cast barCount - 1.0) . (+ ax * dt * 12.0))
  held <- readIORef w.southHeld
  let down = pressed South g
  when (down && not held) $ do
    c <- readIORef w.cursor
    strike au snd w t (cast c)
  writeIORef w.southHeld down

--------------------------------------------------------------------------------
-- Drawing

||| Expanding, fading rings over each struck bar.
rippleGizmos : Double -> List (Int, Double) -> GizmoData
rippleGizmos t = concatMap ring
  where
    ring : (Int, Double) -> GizmoData
    ring (i, t0) =
      let age = (t - t0) / 1.2 in
      if age >= 1.0 then [] else
        let c = withAlpha ((1.0 - age) * 0.85) (hsl (barHue i) 0.75 0.7)
            centre = v3 (barX i) 0.06 0.0
         in gCircle c centre (v3 0.0 1.0 0.0) (0.55 + 2.6 * age) 40
         ++ gCircle c centre (v3 0.0 1.0 0.0) (0.35 + 2.6 * age * 0.8) 40

||| A struck bar swells and settles.
pulseFor : Double -> List (Int, Double) -> Int -> Double
pulseFor t rs i = foldl bump 1.0 rs
  where
    bump : Double -> (Int, Double) -> Double
    bump acc (j, t0) =
      if j == i then acc + 0.35 * exp (-5.0 * (t - t0)) else acc

barModel : Double -> List (Int, Double) -> Int -> Mat4
barModel t rs i =
  translate (barX i) 0.0 0.0 `mmul` scaleM (0.48 * pulseFor t rs i)

||| Hovering a bar walks the highlight there; a press (mouse or finger,
||| unified into `PointerId` by the picker) strikes what the ray actually
||| hit -- no screen-space bar arithmetic anywhere.
onPick : Audio au => au -> SoundHandle -> World -> Double
      -> PickEvent Int -> IO ()
onPick au snd w t (PickOver _ i _) = writeIORef w.cursor (cast i)
onPick au snd w t (PickDown _ LeftButton i _) = strike au snd w t i
onPick _ _ _ _ _ = pure ()

||| Each bar carries two assets, made once: its normal self and an
||| emissive-lifted highlight. Which one draws is a pure function of the
||| cursor, so highlighting costs no material writes -- the same reason
||| the renderer retains materials at all.
frame : Renderer r f => Platform p => Audio au =>
        r -> p -> au -> World -> Picker Int
     -> List (Int, Drawable, Drawable) -> MeshHandle Triangles -> Handle StandardMaterial
     -> SoundHandle -> Status -> Double -> L IO ()
frame r p au w picker bars ball markH snd status t = do
  cur <- liftIO $ do
    last <- readIORef w.lastT
    writeIORef w.lastT t
    let dt = clampD 0.0 0.1 (t - last)
    evs <- pollEvents p
    traverse_ (handle r p au snd w status t) evs
    pollPads p au snd w t dt
    modifyIORef w.ripples (filter (\(_, t0) => t - t0 < 1.2))
    rs <- readIORef w.ripples
    -- The bars as pick targets: a unit-sphere bound under exactly the
    -- matrix each bar draws with this frame.
    let targets = map (\(i, _, _) => target i (barModel t rs i)
                                       (BoundSphere zero3 1.0)) bars
    locked <- readIORef w.locked
    sfc <- surfaceSize p
    -- Locked, the pointer has no position: the picker sees no events and
    -- its hover state simply persists.
    picks <- pickEvents picker camera sfc targets (if locked then [] else evs)
    traverse_ (onPick au snd w t) picks
    drawGizmoData r (rippleGizmos t !(readIORef w.ripples))
    readIORef w.cursor
  rs <- liftIO (readIORef w.ripples)
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  -- The bar the cursor is on glows before it is struck, however the
  -- cursor got there: hover, arrows, stick or locked deltas.
  let curBar = the Int (cast cur)
  fr1 <- drawAll r fr (map (\(i, d, dh) =>
           (barModel t rs i, if i == curBar then dh else d)) bars)
  -- The highlight hovers over the (continuous) cursor position.
  fr2 <- draw r fr1 ball markH
           (translate (barX 0 + cur * 1.15) (1.35 + 0.1 * sin (t * 3.0)) 0.0
             `mmul` scaleM 0.16)
  fr3 <- drawGizmos r fr2
  endFrame r fr3

export
run : Renderer r f => Platform p => Audio au =>
      r -> p -> au -> Status -> IO ()
run r p au status = do
  mid <- registerMaterial {m = StandardMaterial} r
  ball <- loadMesh r (sphere 1.0 2)
  bars <- traverse (\i => do
            let hue = barHue i
            h <- addMaterial r mid
                   (withRoughness 0.35 (lit (hsl hue 0.68 0.55)))
            hi <- addMaterial r mid
                    ({ emissive := dim 0.8 (hsl hue 0.8 0.6) }
                     (withRoughness 0.35 (lit (hsl hue 0.68 0.6))))
            pure (i, MkDrawable ball h, MkDrawable ball hi))
          (range 0 (barCount - 1))
  markH <- addMaterial r mid (glowing (dim 1.7 (rgb 1.0 0.85 0.4)))
  snd <- loadSound au (SoundPcm plinkRate plink)
  w <- newWorld
  picker <- newPicker
  status "backend" (rendererName r)
  status "count-label" "gain 1.0"
  status "note" "click/tap a bar; arrows + Space; l locks the pointer"
  runLoop p $ \t => LIO.run (frame r p au w picker bars ball markH snd status t)

export
app : App
app = MkApp "Chime" run
