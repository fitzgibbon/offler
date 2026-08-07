||| Two rows of rotating primitives over a textured ground plane, lit by one
||| directional light -- offler's rendition of bevy's `3d_shapes` example.
||| The front row walks the metallic and roughness ranges (with a blended
||| glass torus, drawn mid-order but composited last by the sorted
||| transparent phase); the back row wears the uv-checker *image* texture,
||| as bevy's `uv_debug_texture` does. The ground demonstrates JPEG
||| decoding; the checker, PNG.
module Examples.Shapes.Scene

import Data.List
import Data.Maybe
import Examples.Assets
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
  (lookingAt (v3 0.0 1.2 (-0.5)) (v3 0.0 1.0 0.0) (at (v3 0.0 6.0 12.5)))

lights : Lights
lights = MkLights (v3 (-0.5) (-1.0) (-0.4)) (rgb 1.0 0.98 0.92) 0.12

groundMat : Maybe TextureHandle -> StandardMaterial
groundMat t =
  case t of
    Just h => withTexture h (withRoughness 0.95 (lit (srgb 0.85 0.87 0.95)))
    Nothing => withRoughness 0.95 (lit (srgb 0.42 0.44 0.50))

||| One material per front-row shape, walking the hue wheel and the metallic
||| and roughness ranges together, so the row also reads as a material
||| chart. The torus (index 2) is blended glass: drawn in row order like
||| everything else, composited after every opaque draw by the sorted
||| transparent phase.
shapeMat : Int -> StandardMaterial
shapeMat i =
  let n = cast i
      base = withMetallic (n * 0.2)
               (withRoughness (0.15 + n * 0.15)
                 (lit (hsl (0.02 + n * 0.17) 0.75 0.55)))
   in if i == 2
        then withAlpha Blend ({ baseColor := withAlpha 0.45 base.baseColor } base)
        else base

||| The back row: matte white wearing the uv-checker image, bevy's
||| `uv_debug_texture` look. The sphere also carries the procedural noise
||| pattern, to show the two compose.
patternMat : Maybe TextureHandle -> Int -> StandardMaterial
patternMat t i =
  let base = withRoughness 0.55 (lit (srgb 0.95 0.95 0.95))
      textured = case t of
                   Just h => withTexture h base
                   Nothing => base
   in if i == 1 then withPattern (Noise 2.8) textured else textured

shapeModel : Double -> (row : Int) -> Int -> (count : Int) -> Mat4
shapeModel t row i count =
  let x = (cast i - cast (count - 1) * 0.5) * 2.4
      z = if row == 0 then 1.8 else -2.4
      spin = if row == 0 then 1.0 else -0.8
   in matOf (withRotation (fromEulerYXZ (t * 0.6 * spin) (t * 0.45 * spin) 0.0)
              (at (v3 x 1.5 z)))

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
drawShapes : Renderer r f => r -> (1 frame : f)
          -> MaterialId StandardMaterial -> (Int -> StandardMaterial)
          -> Double -> (row : Int) -> (count : Int)
          -> List (Int, MeshHandle) -> L1 IO f
drawShapes r fr _ _ _ _ _ [] = pure1 fr
drawShapes r fr mid matFor t row count ((i, m) :: rest) = do
  fr' <- draw r fr mid m (shapeModel t row i count) (matFor i)
  drawShapes r fr' mid matFor t row count rest

handle : Renderer r f => r -> Event -> IO ()
handle r Resized = resize r
handle _ _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> MaterialId StandardMaterial
      -> MeshHandle -> List (Int, MeshHandle)
      -> StandardMaterial -> (Int -> StandardMaterial) -> FpsCounter
      -> Double -> L IO ()
frame r p mid ground shapes gm pm fps t = do
  liftIO (pollEvents p >>= traverse_ (handle r))
  -- No token, no draws: `beginFrame` failing to acquire a surface is not a
  -- flag to remember to test, it is the absence of the thing draws need.
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- draw r fr mid ground identity gm
  fr2 <- drawShapes r fr1 mid shapeMat t 0 (cast (length shapes)) shapes
  fr3 <- drawShapes r fr2 mid pm t 1 (cast (length shapes)) shapes
  endFrame r fr3

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  mid <- registerMaterial {m = StandardMaterial} r
  ground <- loadMesh r (plane 26.0 26.0)
  shapes <- traverse (loadMesh r) shapeMeshes
  let indexed = zip (range 0 (cast (length shapes) - 1)) shapes
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "stats" (show (length shapes * 2) ++ " shapes; back row textured")
  -- Textures decode asynchronously in the browser, so the loop starts in
  -- the innermost continuation, with whatever loaded.
  loadTexture r (FromBase64 marbleJpgMime marbleJpg) $ \marble =>
    loadTexture r (FromBase64 uvCheckerPngMime uvCheckerPng) $ \checker => do
      when (isNothing marble || isNothing checker)
        (setStatus p "note" "texture decode failed")
      runLoop p $ \t =>
        LIO.run (frame r p mid ground indexed
                       (groundMat marble) (patternMat checker) fps t)
          >> reportFps p fps t
