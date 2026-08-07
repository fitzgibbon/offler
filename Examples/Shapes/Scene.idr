||| A row of rotating primitives over a ground plane, lit by one directional
||| light -- offler's rendition of bevy's `3d_shapes` example. Demonstrates
||| multiple meshes, the standard lit material across the metallic and
||| roughness range, and the camera helpers.
module Examples.Shapes.Scene

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

camera : Camera
camera = perspectiveCamera
  (lookingAt (v3 0.0 1.2 0.0) (v3 0.0 1.0 0.0) (at (v3 0.0 4.2 11.0)))

lights : Lights
lights = MkLights (v3 (-0.5) (-1.0) (-0.4)) (rgb 1.0 0.98 0.92) 0.12

groundMat : Material
groundMat = withRoughness 0.95 (lit (srgb 0.42 0.44 0.50))

||| One material per shape, walking the hue wheel and the metallic and
||| roughness ranges together, so the row also reads as a material chart.
shapeMat : Int -> Material
shapeMat i =
  let n = cast i
   in withMetallic (n * 0.2)
        (withRoughness (0.15 + n * 0.15)
          (lit (hsl (0.02 + n * 0.17) 0.75 0.55)))

shapeModel : Double -> Int -> (count : Int) -> Mat4
shapeModel t i count =
  let x = (cast i - cast (count - 1) * 0.5) * 2.4
   in matOf (withRotation (fromEulerYXZ (t * 0.6) (t * 0.45) 0.0)
              (at (v3 x 1.5 0.0)))

||| The shapes, in bevy's line-up.
shapeMeshes : List MeshData
shapeMeshes =
  [ cuboid 1.3 1.3 1.3
  , sphere 0.85 3
  , torus 0.65 0.28 48 24
  , cylinder 0.6 1.3 48
  , cone 0.75 1.4 48
  ]

||| The token is threaded through the recursion: it cannot be duplicated or
||| dropped, so this cannot draw a shape outside the pass it was given.
drawShapes : Renderer r f => r -> (1 frame : f) -> Double -> (count : Int)
          -> List (Int, MeshHandle) -> L1 IO f
drawShapes r fr _ _ [] = pure1 fr
drawShapes r fr t count ((i, m) :: rest) = do
  fr' <- draw r fr m (shapeModel t i count) (shapeMat i)
  drawShapes r fr' t count rest

handle : Renderer r f => r -> Event -> IO ()
handle r Resized = resize r
handle _ _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> MeshHandle -> List (Int, MeshHandle) -> FpsCounter
      -> Double -> L IO ()
frame r p ground shapes fps t = do
  liftIO (pollEvents p >>= traverse_ (handle r))
  -- No token, no draws: `beginFrame` failing to acquire a surface is not a
  -- flag to remember to test, it is the absence of the thing draws need.
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- draw r fr ground identity groundMat
  fr2 <- drawShapes r fr1 t (cast (length shapes)) shapes
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  ground <- loadMesh r (plane 26.0 26.0)
  shapes <- traverse (loadMesh r) shapeMeshes
  let indexed = zip (range 0 (cast (length shapes) - 1)) shapes
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "stats" (show (length shapes) ++ " shapes + ground")
  runLoop p (\t => LIO.run (frame r p ground indexed fps t) >> reportFps p fps t)
