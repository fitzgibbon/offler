||| A particle fountain as an entity-component-system. Two nozzles ride a
||| spinning arm -- a three-entity `ChildOf` hierarchy whose world
||| matrices `propagate` computes -- and every spark is an entity, spawned
||| from a nozzle's `GlobalTransform`, integrated, bounced if it is
||| `Bouncy`, aged, and despawned when its life runs out: thousands of
||| spawns and despawns a second through generational indices. The sparks
||| draw as *one* instanced call straight from their `Instance` column,
||| which is the GPU staging buffer with nothing copied out of it.
|||
||| The rest of the ECS shows too: the emission rate is a resource, input
||| arrives as `Events Event`, a click ray-picks a nozzle through its
||| `Pickable` bound and toggles a `Paused` tag, and the status line counts
||| `Added Life` -- this frame's newborn sparks, by change detection. One
||| `Schedule` runs it all. `+`/`-` double and halve the emission rate.
module Examples.Fountain.Scene

import Data.IORef
import Examples.Util
import Offler.Camera
import Offler.Color
import Offler.Ecs
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

--------------------------------------------------------------------------------
-- Components

record Pos where
  constructor MkPos
  pos : V3

record Vel where
  constructor MkVel
  vel : V3

record Life where
  constructor MkLife
  age, span : Double

||| A spark's base hue, from its nozzle.
record Hue where
  constructor MkHue
  hue : Double

||| A nozzle, and the hue its sparks take.
record Nozzle where
  constructor MkNozzle
  nozzleHue : Double

||| Spins its `Transform` about the local y axis.
record Spinner where
  constructor MkSpinner
  rate : Double

||| Sparks that bounce off the floor rather than falling through it.
data Bouncy = MkBouncy

||| The instanced batch the sparks draw in.
data Spark : Type where

Columnar Pos where
  width = 3
  storeCol a o (MkPos p) = do
    poke a (field 0 o) p.vx; poke a (field 1 o) p.vy; poke a (field 2 o) p.vz
  loadCol a o = do
    x <- peek a (field 0 o); y <- peek a (field 1 o); z <- peek a (field 2 o)
    pure (MkPos (MkV3 x y z))

Columnar Vel where
  width = 3
  storeCol a o (MkVel v) = do
    poke a (field 0 o) v.vx; poke a (field 1 o) v.vy; poke a (field 2 o) v.vz
  loadCol a o = do
    x <- peek a (field 0 o); y <- peek a (field 1 o); z <- peek a (field 2 o)
    pure (MkVel (MkV3 x y z))

Columnar Life where
  width = 2
  storeCol a o (MkLife ag sp) = poke a (field 0 o) ag >> poke a (field 1 o) sp
  loadCol a o = [| MkLife (peek a (field 0 o)) (peek a (field 1 o)) |]

Columnar Hue where
  width = 1
  storeCol a o (MkHue h) = poke a o h
  loadCol a o = MkHue <$> peek a o

Columnar Nozzle where
  width = 1
  storeCol a o (MkNozzle h) = poke a o h
  loadCol a o = MkNozzle <$> peek a o

Columnar Spinner where
  width = 1
  storeCol a o (MkSpinner r) = poke a o r
  loadCol a o = MkSpinner <$> peek a o

||| A nozzle switched off by a click: a tag, so it costs nothing per spark.
data Paused = MkPaused

||| A nozzle's two drawables, lit and dark: what a click swaps between.
record Looks where
  constructor MkLooks
  lit, dark : Drawable

Component Pos where kind = Packed
Component Vel where kind = Packed
Component Life where kind = Packed
Component Hue where kind = Packed
Component Nozzle where kind = Packed
Component Spinner where kind = Packed
Component Bouncy where kind = Marker MkBouncy
Component Paused where kind = Marker MkPaused
Component Looks where kind = Boxed

-- Resources: one value for the world, read and written by systems.

||| Sparks per second per nozzle.
record Rate where
  constructor MkRate
  perSecond : Double

||| The fractional spark carried to the next tick.
record Owed where
  constructor MkOwed
  owed : Double

||| How many sparks have been made: the seed of each one's scatter.
record Serial where
  constructor MkSerial
  serial : Int

Resource Rate where initial = MkRate 2500.0
Resource Owed where initial = MkOwed 0.0
Resource Serial where initial = MkSerial 0

||| Everything this world can hold. A system naming anything else does not
||| compile.
W : List Type
W = [ Transform, GlobalTransform, ChildOf, Renderable, Pickable, Spinner
    , Nozzle, Paused, Looks, Pos, Vel, Life, Hue, Bouncy, Instance Spark
    , Rate, Owed, Serial, Events Event ]

maxEntities : Int
maxEntities = 65536

--------------------------------------------------------------------------------
-- Systems

camera : Camera
camera = withClearColor (rgb 0.015 0.018 0.03) $ perspectiveCamera
  (lookingAt (v3 0.0 2.4 0.0) (v3 0.0 1.0 0.0) (at (v3 0.0 6.5 15.0)))

gravity : Double
gravity = 9.0

||| A deterministic hash in [0, 1): the sparks' scatter.
hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

spin : System W
spin t w = cmap {q = (Spinner, Transform)} w $ \(MkSpinner r, tf) =>
  withRotation (qNormalize (axisAngle (v3 0.0 1.0 0.0) (r * t.delta) `qmul` tf.rotation)) tf

||| `k` sparks from a nozzle's world matrix, numbered from `i`; returns
||| the next number.
spray : Mat4 -> Nozzle -> (k : Int) -> (i : Int) -> World W -> IO Int
spray m (MkNozzle h) k i w =
  if k <= 0 then pure i else do
    let up = normalize3 (v3 m.m4 m.m5 m.m6)
        tip = add3 (v3 m.m12 m.m13 m.m14) (scale3 0.55 up)
        jitter = v3 (hashUnit (i * 3) - 0.5) (hashUnit (i * 3 + 1) - 0.5) (hashUnit (i * 3 + 2) - 0.5)
        v = add3 (scale3 (7.0 + 2.0 * hashUnit (i * 7)) up) (scale3 1.6 jitter)
        life = MkLife 0.0 (2.2 + 1.6 * hashUnit (i * 5))
        spark = (MkPos tip, MkVel v, life, MkHue (h + 0.08 * (hashUnit (i * 11) - 0.5)))
    _ <- if i `mod` 3 == 0
           then spawn w (spark, MkBouncy)
           else spawn w spark
    spray m (MkNozzle h) (k - 1) (i + 1) w

||| Spawn this tick's share of sparks at every running nozzle, carrying the
||| fractional remainder to the next tick.
emit : System W
emit t w = do
  MkRate r <- resource {c = Rate} w
  MkOwed owed <- resource {c = Owed} w
  let due = owed + r * t.delta
      k = the Int (cast (floor due))
  setResource w (MkOwed (due - cast k))
  cfor {q = (GlobalTransform, Nozzle, Without Paused)} w $ \(MkGlobal m, nz, _) => do
    MkSerial i <- resource {c = Serial} w
    spray m nz k i w >>= setResource w . MkSerial

move : System W
move t w = cmap {q = (Pos, Vel)} w $ \(MkPos p, MkVel v) =>
  let v' = { vy $= \y => y - gravity * t.delta } v
   in (MkPos (add3 p (scale3 t.delta v')), MkVel v')

bounce : System W
bounce _ w = cmap {q = (With Bouncy, Pos, Vel)} w $ \(_, MkPos p, MkVel v) =>
  if p.vy < 0.0 && v.vy < 0.0
    then (MkPos ({ vy := 0.0 } p), MkVel (MkV3 (v.vx * 0.8) (-v.vy * 0.6) (v.vz * 0.8)))
    else (MkPos p, MkVel v)

||| Age every spark and despawn the spent ones -- from inside the query
||| over them, which the snapshot iteration makes safe.
age : System W
age t w = cmapM {q = (Entity, Life)} w $ \(e, MkLife a s) =>
  if a + t.delta >= s
    then despawn w e >> pure (MkLife a s)
    else pure (MkLife (a + t.delta) s)

||| Each spark's instance: shrinking and cooling as it ages.
paint : System W
paint _ w = cmap {q = (Pos, Life, Hue)} {r = Instance Spark} w $ \(MkPos p, MkLife a s, MkHue h) =>
  let k = a / s
      size = 0.06 * (1.0 - 0.6 * k)
   in MkInstance (translate p.vx p.vy p.vz `mmul` scaleM size)
                 (hsl (h + 0.12 * k) 0.85 (0.62 - 0.3 * k))

||| `+`/`-` from this frame's input events: double or halve the rate.
controls : Status -> System W
controls status _ w = do
  evs <- readEvents Event w
  traverse_ key evs
  where
    set : Double -> IO ()
    set n = do
      let n' = max 250.0 (min 16000.0 n)
      setResource w (MkRate n')
      status "note" ("+/- emission: " ++ show (the Int (cast n')) ++ " sparks/s per nozzle; click a nozzle to stop it")

    key : Event -> IO ()
    key (KeyDown k) = do
      MkRate r <- resource {c = Rate} w
      if k == "+" || k == "="
        then set (r * 2.0)
        else if k == "-" || k == "_" then set (r / 2.0) else pure ()
    key _ = pure ()

||| A click on a nozzle -- ray-picked against its `Pickable` bound at its
||| `GlobalTransform` -- toggles its `Paused` tag and swaps its look.
clicks : Platform p => p -> Picker Entity -> System W
clicks p picker _ w = do
  evs <- readEvents Event w
  targets <- pickTargets w
  sfc <- surfaceSize p
  picks <- pickEvents picker camera sfc targets evs
  traverse_ toggle picks
  where
    toggle : PickEvent Entity -> IO ()
    toggle (PickClick _ _ e _) = do
      Just looks <- getComponent {c = Looks} w e | Nothing => pure ()
      paused <- holds Paused w e
      if paused
        then removeComponent Paused w e >> insert w e (MkRenderable looks.lit)
        else insert w e MkPaused >> insert w e (MkRenderable looks.dark)
    toggle _ = pure ()

||| The live count, and how many sparks were added since this system last
||| ran -- one frame's worth, by change detection.
stats : Status -> System W
stats status _ w = do
  n <- countOf (Instance Spark) w
  born <- cfold {q = Added Life} w (\k, _ => k + 1) (the Int 0)
  status "count-label" (show n ++ " sparks, +" ++ show born ++ " this frame")

systems : Platform p => Status -> p -> Picker Entity -> List (System W)
systems status p picker =
  [ controls status, spin, \_ => propagate, clicks p picker
  , emit, move, bounce, age, paint, stats status ]

--------------------------------------------------------------------------------
-- The scene

lights : Lights
lights = Offler.Light.lights 0.22
  [ MkDirectional (v3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.95 0.88)
  , MkDirectional (v3 0.6 (-0.3) 0.5) (rgb 0.25 0.3 0.45) ]

prop : Drawable -> Transform -> Maybe Entity -> World W -> IO (Maybe Entity)
prop d tf Nothing w = spawn w (tf, MkRenderable d)
prop d tf (Just p) w = spawn w (tf, MkRenderable d, MkChildOf p)

||| The ground, a post, and the spinning arm with a nozzle at each end:
||| post <- arm <- nozzles, three levels of `ChildOf`.
build : Renderer r f => r -> MaterialId StandardMaterial -> World W -> IO ()
build r mid w = do
  ground <- loadMesh r (plane 30.0 30.0)
  post <- loadMesh r (cylinder 0.25 2.4 32)
  arm <- loadMesh r (cuboid 4.6 0.22 0.22)
  nozzle <- loadMesh r (cone 0.28 0.6 32)
  groundH <- addMaterial r mid (withRoughness 0.9 (lit (rgb 0.10 0.11 0.14)))
  metalH <- addMaterial r mid (withMetallic 0.8 (withRoughness 0.35 (lit (rgb 0.55 0.58 0.62))))
  hotH <- addMaterial r mid (glowing (dim 1.4 (rgb 1.0 0.55 0.2)))
  coolH <- addMaterial r mid (glowing (dim 1.4 (rgb 0.25 0.6 1.0)))
  darkH <- addMaterial r mid (withRoughness 0.5 (lit (rgb 0.12 0.12 0.14)))
  _ <- prop (MkDrawable ground groundH) neutral Nothing w
  Just base <- prop (MkDrawable post metalH) (at (v3 0.0 1.2 0.0)) Nothing w
    | Nothing => pure ()
  Just armE <- prop (MkDrawable arm metalH) (at (v3 0.0 1.2 0.0)) (Just base) w
    | Nothing => pure ()
  insert w armE (MkSpinner 0.9)
  let tilt = \a => withRotation (axisAngle (v3 0.0 0.0 1.0) a)
      dark = MkDrawable nozzle darkH
      nozzleAt = \x, a, h, look => do
        Just n <- prop look (tilt a (at (v3 x 0.0 0.0))) (Just armE) w
          | Nothing => pure ()
        insertAll w n (MkNozzle h, MkLooks look dark, MkPickable (BoundSphere zero3 0.6))
  nozzleAt 2.3 (-0.45) 0.06 (MkDrawable nozzle hotH)
  nozzleAt (-2.3) 0.45 0.58 (MkDrawable nozzle coolH)

frame : Renderer r f => r -> World W -> Schedule W -> Clock
     -> MeshHandle Triangles -> Handle StandardMaterial -> InstanceHandle
     -> List Event -> Double -> L IO ()
frame r w sch clock sparkM sparkH ih evs t = do
  liftIO $ do
    -- Input becomes events in the world, read by whichever systems care.
    sendEvents w evs
    tk <- tick clock t
    runSchedule sch tk w
    uploadBatch Spark r ih w
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- renderWorld r fr w
  fr2 <- drawInstanced r fr1 sparkM sparkH ih identity
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  w <- newWorld {cs = W} maxEntities
  mid <- registerMaterial {m = StandardMaterial} r
  build r mid w
  sparkM <- loadMesh r (sphere 1.0 1)
  -- One asset for every spark: the colour is per instance.
  sparkH <- addMaterial r mid (unlit white)
  ih <- createInstances r
  picker <- newPicker
  sch <- newSchedule (systems status p picker)
  clock <- newClock 0.1
  fps <- newFps
  status "backend" (rendererName r)
  status "note" "+/- emission: 2500 sparks/s per nozzle; click a nozzle to stop it"
  runLoop p $ \t => do
    evs <- pollEvents p
    LIO.run (frame r w sch clock sparkM sparkH ih evs t)
    reportFps status fps t

export
app : App
app = MkApp "Fountain" (soundless run)
