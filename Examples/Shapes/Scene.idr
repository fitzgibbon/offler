||| Two rows of rotating primitives over a textured ground plane -- offler's
||| rendition of bevy's `3d_shapes`, now bevy-shaped inside as well: meshes,
||| textures and material assets are created once at startup, the objects
||| live in a retained scene graph, and the per-frame work is setting node
||| transforms and rendering the scene. The torus is an *indexed* mesh; the
||| front row walks the metallic/roughness ranges (index 2 blended glass);
||| the back row wears the PNG uv-checker, bevy's `uv_debug_texture` look;
||| the ground decodes from JPEG. Two directional lights.
module Examples.Shapes.Scene

import Data.List
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
import Offler.Scene
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

camera : Camera
camera = perspectiveCamera
  (lookingAt (v3 0.0 1.2 (-0.5)) (v3 0.0 1.0 0.0) (at (v3 0.0 6.0 12.5)))

lights : Lights
lights = twoLights

||| One material per front-row shape, walking the hue wheel and the
||| metallic and roughness ranges together. The torus (index 2) is blended
||| glass: drawn in scene order, composited after every opaque draw by the
||| sorted transparent phase.
shapeMat : Int -> StandardMaterial
shapeMat i =
  let n = cast i
      base = withMetallic (n * 0.2)
               (withRoughness (0.15 + n * 0.15)
                 (lit (hsl (0.02 + n * 0.17) 0.75 0.55)))
   in if i == 2
        then withAlpha Blend ({ baseColor := withAlpha 0.45 base.baseColor } base)
        else base

||| The back row: matte white wearing the uv-checker image. The sphere also
||| carries the procedural noise pattern, to show the two compose.
patternMat : TextureHandle -> Int -> StandardMaterial
patternMat checker i =
  let base = withTexture checker (withRoughness 0.55 (lit (srgb 0.95 0.95 0.95)))
   in if i == 1 then withPattern (Noise 2.8) base else base

shapeTransform : Double -> (row : Int) -> Int -> (count : Int) -> Transform
shapeTransform t row i count =
  let x = (cast i - cast (count - 1) * 0.5) * 2.4
      z = if row == 0 then 1.8 else -2.4
      spin = if row == 0 then 1.0 else -0.8
   in withRotation (fromEulerYXZ (t * 0.6 * spin) (t * 0.45 * spin) 0.0)
        (at (v3 x 1.5 z))

handle : Event -> IO ()
handle _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> Scene -> List (NodeId, Int, Int, Int) -> FpsCounter
      -> Status -> Double -> L IO ()
frame r p sc animated fps status t = do
  liftIO $ do
    pollEvents p >>= traverse_ handle
    -- The per-frame work of a retained scene: move the nodes.
    traverse_ (\(n, row, i, count) =>
                 setTransform sc n (shapeTransform t row i count))
              animated
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- renderScene r fr sc
  endFrame r fr1

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  -- Textures return immediately -- white until the browser's decode lands.
  marble <- loadTexture r (FromBase64 marbleJpgMime marbleJpg)
  checker <- loadTexture r (FromBase64 uvCheckerPngMime uvCheckerPng)
  -- Meshes: the torus indexed, the rest soup.
  ground <- loadMesh r (plane 26.0 26.0)
  cubeM <- loadMesh r (cuboid 1.3 1.3 1.3)
  sphereM <- loadMesh r (sphere 0.85 3)
  torusM <- loadIndexed r (torusIndexed 0.65 0.28 48 24)
  cylM <- loadMesh r (cylinder 0.6 1.3 48)
  coneM <- loadMesh r (cone 0.75 1.4 48)
  let meshes = [cubeM, sphereM, torusM, cylM, coneM]
      count = cast (length meshes)
  -- Material assets: uploaded once, referenced by handle ever after.
  groundH <- addMaterial r mid
               (withTexture marble (withRoughness 0.95 (lit (srgb 0.85 0.87 0.95))))
  frontHs <- traverse (addMaterial r mid . shapeMat) (range 0 (count - 1))
  backHs <- traverse (addMaterial r mid . patternMat checker) (range 0 (count - 1))
  -- The scene: a ground node and ten shape nodes.
  sc <- newScene
  _ <- spawn sc Nothing neutral (Just (MkDrawable ground groundH))
  let rows = the (List (Int, List (Handle StandardMaterial))) [(0, frontHs), (1, backHs)]
  animated <- traverse (\(row, hs) =>
                 traverse (\(i, mesh, h) => do
                     n <- spawn sc Nothing (shapeTransform 0.0 row i count)
                                (Just (MkDrawable mesh h))
                     pure (n, row, i, count))
                   (zip3 (range 0 (count - 1)) meshes hs))
               rows
  fps <- newFps
  status "backend" (rendererName r)
  status "stats" (show (count * 2) ++ " shapes; indexed torus; 2 lights")
  runLoop p $ \t =>
    LIO.run (frame r p sc (concat animated) fps status t)
      >> reportFps status fps t

export
app : App
app = MkApp "Shapes" (soundless run)
