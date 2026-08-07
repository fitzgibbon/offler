||| Tens of thousands of bodies swarming a glowing core, in *one*
||| instanced draw: the model matrix and colour stream at instance rate --
||| offler's spelling of bevy's automatic batching, gated by `InstOk` at
||| compile time instead of discovered by the render graph. A frame fills
||| the persistent instance staging buffer (a trampolined loop, no
||| intermediate list), uploads it once, and issues one call however large
||| the crowd. `+`/`-` halve and double the count, to 200 000.
module Examples.Swarm.Scene

import Data.IORef
import Examples.Util
import Offler.Camera
import Offler.Color
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math
import Offler.Mesh
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

||| Orbit parameters and colour for one body.
record Body where
  constructor MkBody
  radius, rate, phase, incline, size : Double
  tint : Color

hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

sizeFor : Int -> Double
sizeFor n = max 0.03 (0.55 / exp (0.26 * log (cast (max 1 n))))

mkBody : (count : Int) -> (i : Int) -> Body
mkBody count i =
  let frac = if count <= 1 then 0.0 else cast i / cast (count - 1)
      radius = 2.4 + 6.3 * sqrt frac
   in MkBody
        radius
        (0.9 / (0.6 + radius * 0.3))
        (hashUnit (i * 5 + 9) * tau)
        ((hashUnit (i * 11 + 3) - 0.5) * 0.5)
        (sizeFor count * (0.6 + 0.8 * hashUnit (i * 13 + 5)))
        (hsl (hashUnit (i * 3 + 2)) 0.7 0.55)

||| Built with an accumulator: a plain `map` at two hundred thousand
||| elements is a stack overflow on the JS backend, a self tail call is a
||| trampoline.
bodiesFor : Int -> List Body
bodiesFor count = go (count - 1) []
  where
    go : Int -> List Body -> List Body
    go i acc = if i < 0 then acc else go (i - 1) (mkBody count i :: acc)

bodyModel : Double -> Body -> Mat4
bodyModel t b =
  let a = t * b.rate + b.phase
      x = cos a * b.radius
      z = sin a * b.radius
      y = sin (a * 2.0) * b.radius * b.incline
   in translate x y z `mmul` scaleM b.size

coreModel : Double -> Mat4
coreModel t = rotateY (t * 0.1) `mmul` scaleM 1.2

camera : Camera
camera = perspectiveCamera
  (lookingAt zero3 (v3 0.0 1.0 0.0) (at (v3 0.0 7.5 14.5)))

lights : Lights
lights = MkLights 0.18 [MkDirectional (v3 (-0.3) (-1.0) (-0.25)) (rgb 1.0 0.95 0.9)]

maxBodies : Int
maxBodies = 200000

record World where
  constructor MkWorld
  count : IORef Int
  bodies : IORef (List Body)

applyCount : Status -> World -> Int -> IO ()
applyCount status w wanted = do
  let n = max 1 (min maxBodies wanted)
  writeIORef w.count n
  writeIORef w.bodies (bodiesFor n)
  status "count-label" (show n ++ (if n == 1 then " body" else " bodies"))

handle : Status -> World -> Event -> IO ()
handle status w (KeyDown k) = do
  n <- readIORef w.count
  if k == "+" || k == "="
    then applyCount status w (n * 2)
    else if k == "-" || k == "_"
      then applyCount status w (n `div` 2)
      else pure ()
handle _ _ _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> World
      -> MeshHandle Triangles -> Handle StandardMaterial
      -> MeshHandle Triangles -> Handle StandardMaterial
      -> InstanceHandle -> InstBuf
      -> FpsCounter -> Status -> Double -> L IO ()
frame r p w core coreH body bodyH ih ib fps status t = do
  liftIO $ do
    pollEvents p >>= traverse_ (handle status w)
    bodies <- readIORef w.bodies
    -- Fill the persistent staging buffer straight from the body list --
    -- no intermediate per-frame list -- and upload it once.
    sl <- fillInstances ib (\b => (bodyModel t b, b.tint)) bodies
    writeInstances r ih sl
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- draw r fr core coreH (coreModel t)
  -- The whole crowd: one call, one object slot.
  fr2 <- drawInstanced r fr1 body bodyH ih identity
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  core <- loadMesh r (sphere 1.0 3)
  body <- loadMesh r (sphere 1.0 1)
  coreH <- addMaterial r mid (glowing (dim 2.2 (rgb 1.0 0.72 0.30)))
  -- One asset for every body: the colour is per *instance*.
  bodyH <- addMaterial r mid (withRoughness 0.6 (lit white))
  ih <- createInstances r
  ib <- newInstBuf maxBodies
  w <- MkWorld <$> newIORef 0 <*> newIORef []
  applyCount status w 50000
  fps <- newFps
  status "backend" (rendererName r)
  status "note" "+/- to double/halve"
  runLoop p $ \t =>
    LIO.run (frame r p w core coreH body bodyH ih ib fps status t)
      >> reportFps status fps t

export
app : App
app = MkApp "Swarm" (soundless run)
