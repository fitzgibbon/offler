||| Thousands of bodies swarming a glowing core, drawn with one `drawMany`
||| batch per frame. Demonstrates batched drawing at scale -- the whole crowd
||| costs a handful of linear binds however large it grows -- and the
||| emissive material. `+`/`-` halve and double the count, to 10 000.
module Examples.Swarm.Scene

import Data.IORef
import Data.List
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

||| Orbit radius, rate, phase, inclination, size and colour for one body.
record Body where
  constructor MkBody
  radius, rate, phase, incline, size : Double
  mat : StandardMaterial

||| Deterministic pseudo-random in [0,1), so the swarm is stable from frame
||| to frame and for a given index as the count changes.
hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

||| Bodies shrink as the crowd grows, or they merge into one blob.
sizeFor : Int -> Double
sizeFor n = max 0.045 (0.55 / exp (0.26 * log (cast (max 1 n))))

||| `sqrt` of the index fraction spreads bodies evenly by area rather than by
||| radius, so the disc does not bunch up towards the middle.
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
        (withRoughness 0.6 (lit (hsl (hashUnit (i * 3 + 2)) 0.7 0.55)))

buildBodies : Int -> List Body
buildBodies count = map (mkBody count) (range 0 (count - 1))

bodyModel : Double -> Body -> Mat4
bodyModel t b =
  let a = t * b.rate + b.phase
      x = cos a * b.radius
      z = sin a * b.radius
      y = sin (a * 2.0) * b.radius * b.incline
   in translate x y z `mmul` scaleM b.size

coreMat : StandardMaterial
coreMat = glowing (dim 2.2 (rgb 1.0 0.72 0.30))

coreModel : Double -> Mat4
coreModel t = rotateY (t * 0.1) `mmul` scaleM 1.2

camera : Camera
camera = perspectiveCamera
  (lookingAt zero3 (v3 0.0 1.0 0.0) (at (v3 0.0 7.5 14.5)))

lights : Lights
lights = MkLights (v3 (-0.3) (-1.0) (-0.25)) (rgb 1.0 0.95 0.9) 0.18

record World where
  constructor MkWorld
  count : IORef Int
  bodies : IORef (List Body)

applyCount : Platform p => p -> World -> Int -> IO ()
applyCount p w wanted = do
  let n = max 1 (min 10000 wanted)
  writeIORef w.count n
  writeIORef w.bodies (buildBodies n)
  setStatus p "count-label" (show n ++ (if n == 1 then " body" else " bodies"))

handle : Renderer r f => Platform p => r -> p -> World -> Event -> IO ()
handle r _ _ Resized = resize r
handle _ p w (KeyDown k) = do
  n <- readIORef w.count
  if k == "+" || k == "="
    then applyCount p w (n * 2)
    else if k == "-" || k == "_"
      then applyCount p w (n `div` 2)
      else pure ()
handle _ _ _ _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> World -> MaterialId StandardMaterial
      -> MeshHandle -> MeshHandle -> FpsCounter
      -> Double -> L IO ()
frame r p w mid core body fps t = do
  liftIO (pollEvents p >>= traverse_ (handle r p w))
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- draw r fr mid core (coreModel t) coreMat
  bodies <- liftIO (readIORef w.bodies)
  -- One bind for the whole crowd, not one per body: `drawMany` loops inside
  -- a single lifted IO action, so ten thousand bodies do not put ten
  -- thousand `Bind` frames on the JS engine's stack.
  fr2 <- drawMany r fr1 mid body (map (\b => (bodyModel t b, b.mat)) bodies)
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  mid <- registerMaterial {m = StandardMaterial} r
  core <- loadMesh r (sphere 1.0 3)
  body <- loadMesh r (sphere 1.0 1)
  w <- MkWorld <$> newIORef 0 <*> newIORef []
  applyCount p w 2000
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "note" "+/- to double/halve"
  runLoop p (\t => LIO.run (frame r p w mid core body fps t) >> reportFps p fps t)
