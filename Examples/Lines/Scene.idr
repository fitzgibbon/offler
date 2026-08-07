||| Line-list rendering: a spinning wireframe globe, an animated Lissajous
||| ribbon with two glowing markers riding it, and a ground grid -- all drawn
||| as one blended overlay in a single call, over ordinary lit meshes.
||| Demonstrates `setLines`/`drawLines`, per-frame geometry rebuilds, and
||| mixing the two pipelines in one pass.
module Examples.Lines.Scene

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

camera : Double -> Camera
camera t =
  let yaw = t * 0.1
      eye = v3 (11.0 * sin yaw) 4.5 (11.0 * cos yaw)
   in perspectiveCamera (lookingAt (v3 0.0 1.8 0.0) (v3 0.0 1.0 0.0) (at eye))

lights : Lights
lights = defaultLights

lineColor : Color
lineColor = srgba 0.62 0.72 0.95 0.45

markerMat : StandardMaterial
markerMat = glowing (dim 2.5 (rgb 1.0 0.62 0.25))

||| The globe: every triangle edge of a subdivided icosahedron, spun about +y
||| and lifted over the grid. Shared edges appear twice, which the blend
||| makes brighter rather than wrong.
globe : Double -> List (V3, V3)
globe t =
  let q = axisAngle (v3 0.0 1.0 0.0) (t * 0.35)
      place = \v => add3 (qRotate q (scale3 1.6 v)) (v3 0.0 2.2 0.0)
   in concatMap (\(a, b, c) =>
        let pa = place a.position
            pb = place b.position
            pc = place c.position
         in [(pa, pb), (pb, pc), (pc, pa)])
        (sphere 1.0 2)

||| Where the ribbon sits at parameter `s`, phases drifting with time.
ribbonPoint : Double -> Double -> V3
ribbonPoint t s =
  v3 (4.2 * sin (2.0 * s + t * 0.31))
     (2.1 + 1.5 * sin (3.0 * s + t * 0.47))
     (4.2 * sin (s + t * 0.13))

ribbon : Double -> List (V3, V3)
ribbon t =
  polyline (map (\k => ribbonPoint t (cast k * (tau / 240.0))) (range 0 240))

overlay : Double -> List (V3, V3)
overlay t = gridXZ 8 1.0 ++ globe t ++ ribbon t

markerModel : Double -> Double -> Mat4
markerModel t phase =
  let pt = ribbonPoint t (t * 0.15 + phase)
   in matOf (uniformScale 0.16 (at pt))

handle : Renderer r f => r -> Event -> IO ()
handle r Resized = resize r
handle _ _ = pure ()

frame : Renderer r f => Platform p =>
        r -> p -> MaterialId StandardMaterial -> MeshHandle -> FpsCounter
      -> Double -> L IO ()
frame r p mid marker fps t = do
  liftIO $ do
    pollEvents p >>= traverse_ (handle r)
    -- Rebuilt on the CPU every frame and re-uploaded, *outside* the pass:
    -- replacing the overlay buffer mid-pass would destroy a buffer the
    -- recorded commands still name.
    loadLines r (overlay t)
  Just fr <- beginFrame r (camera t) lights t
    | Nothing => pure ()
  fr1 <- draw r fr mid marker (markerModel t 0.0) markerMat
  fr2 <- draw r fr1 mid marker (markerModel t 3.1) markerMat
  -- After the meshes, so the blend has something solid to sit over.
  fr3 <- drawLines r fr2 lineColor
  endFrame r fr3

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  mid <- registerMaterial {m = StandardMaterial} r
  marker <- loadMesh r (sphere 1.0 2)
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "stats" (show (length (overlay 0.0)) ++ " segments/frame")
  runLoop p (\t => LIO.run (frame r p mid marker fps t) >> reportFps p fps t)
