||| Thousands of bodies swarming a glowing core. Retained materials make
||| the crowd cheap: bodies share two dozen hue-bucketed material *assets*
||| created once, so a frame writes only model matrices -- one batched
||| `drawMany` per bucket, a handful of linear binds however large the
||| crowd. `+`/`-` halve and double the count, to 10 000.
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

||| Hue buckets: one retained material asset each.
buckets : Int
buckets = 24

bucketMat : Int -> StandardMaterial
bucketMat k =
  withRoughness 0.6 (lit (hsl (cast k / cast buckets) 0.7 0.55))

||| Orbit parameters for one body.
record Body where
  constructor MkBody
  radius, rate, phase, incline, size : Double

hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

sizeFor : Int -> Double
sizeFor n = max 0.045 (0.55 / exp (0.26 * log (cast (max 1 n))))

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

||| Which bucket a body's hue falls in: the same hash the colours use.
bucketOf : Int -> Int
bucketOf i = cast (hashUnit (i * 3 + 2) * cast buckets) `mod` buckets

||| Bodies grouped by bucket, computed once per count change: the retained
||| structure a frame maps over.
grouped : Int -> List (Int, List Body)
grouped count =
  map (\k => (k, map (mkBody count)
                     (filter (\i => bucketOf i == k) (range 0 (count - 1)))))
      (range 0 (buckets - 1))

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

record World where
  constructor MkWorld
  count : IORef Int
  groups : IORef (List (Int, List Body))

applyCount : Status -> World -> Int -> IO ()
applyCount status w wanted = do
  let n = max 1 (min 10000 wanted)
  writeIORef w.count n
  writeIORef w.groups (grouped n)
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

||| One `drawMany` per bucket: the token threads through two dozen binds
||| however many thousand bodies there are.
drawGroups : Renderer r f => r -> (1 frame : f)
          -> MeshHandle Triangles -> List (Handle StandardMaterial)
          -> Double -> List (Int, List Body) -> L1 IO f
drawGroups r fr _ _ _ [] = pure1 fr
drawGroups r fr mesh hs t ((k, bodies) :: rest) =
  case inBounds k hs of
    Nothing => drawGroups r fr mesh hs t rest
    Just h => do
      fr' <- drawMany r fr mesh h (map (bodyModel t) bodies)
      drawGroups r fr' mesh hs t rest
  where
    inBounds : Int -> List (Handle StandardMaterial) -> Maybe (Handle StandardMaterial)
    inBounds _ [] = Nothing
    inBounds 0 (x :: _) = Just x
    inBounds n (_ :: xs) = inBounds (n - 1) xs

frame : Renderer r f => Platform p =>
        r -> p -> World
      -> MeshHandle Triangles -> Handle StandardMaterial
      -> MeshHandle Triangles -> List (Handle StandardMaterial)
      -> FpsCounter -> Status -> Double -> L IO ()
frame r p w core coreH body bodyHs fps status t = do
  liftIO (pollEvents p >>= traverse_ (handle status w))
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- draw r fr core coreH (coreModel t)
  groups <- liftIO (readIORef w.groups)
  fr2 <- drawGroups r fr1 body bodyHs t groups
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  core <- loadMesh r (sphere 1.0 3)
  body <- loadMesh r (sphere 1.0 1)
  coreH <- addMaterial r mid (glowing (dim 2.2 (rgb 1.0 0.72 0.30)))
  bodyHs <- traverse (addMaterial r mid . bucketMat) (range 0 (buckets - 1))
  w <- MkWorld <$> newIORef 0 <*> newIORef []
  applyCount status w 2000
  fps <- newFps
  status "backend" (rendererName r)
  status "note" "+/- to double/halve"
  runLoop p $ \t =>
    LIO.run (frame r p w core coreH body bodyHs fps status t)
      >> reportFps status fps t
