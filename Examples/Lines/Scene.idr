||| Line-topology meshes as first-class citizens: a retained wireframe
||| globe and ground grid are `Lines` meshes in the scene graph, spun by
||| their node transforms -- no per-frame geometry rebuild -- drawn through
||| the standard material's `vs_line` entry (the type checker demands that
||| entry exist: `TopoOk`). The animated Lissajous ribbon stays on the
||| immediate-mode gizmo overlay, offler's `bevy_gizmos`, rebuilt each
||| frame because its geometry genuinely changes.
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
import Offler.Scene
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

||| The globe's edges, in *object* space: the node's transform spins it.
globeSegments : List (V3, V3)
globeSegments =
  concatMap (\(a, b, c) =>
      [ (a.position, b.position)
      , (b.position, c.position)
      , (c.position, a.position) ])
    (sphere 1.6 2)

ribbonPoint : Double -> Double -> V3
ribbonPoint t s =
  v3 (4.2 * sin (2.0 * s + t * 0.31))
     (2.1 + 1.5 * sin (3.0 * s + t * 0.47))
     (4.2 * sin (s + t * 0.13))

ribbon : Double -> List (V3, V3)
ribbon t =
  polyline (map (\k => ribbonPoint t (cast k * (tau / 240.0))) (range 0 240))

markerTransform : Double -> Double -> Transform
markerTransform t phase =
  uniformScale 0.16 (at (ribbonPoint t (t * 0.15 + phase)))

globeTransform : Double -> Transform
globeTransform t =
  withRotation (axisAngle (v3 0.0 1.0 0.0) (t * 0.35)) (at (v3 0.0 2.2 0.0))

frame : Renderer r f => Platform p =>
        r -> p -> Scene -> (globe, m1, m2 : NodeId) -> FpsCounter
      -> Status -> Double -> L IO ()
frame r p sc globe m1 m2 fps status t = do
  liftIO $ do
    _ <- pollEvents p
    setTransform sc globe (globeTransform t)
    setTransform sc m1 (markerTransform t 0.0)
    setTransform sc m2 (markerTransform t 3.1)
    -- The gizmo overlay is the one thing rebuilt per frame: its geometry
    -- actually changes. Re-uploaded outside the pass.
    loadLines r (ribbon t)
  Just fr <- beginFrame r (camera t) lights t
    | Nothing => pure ()
  fr1 <- renderScene r fr sc
  -- After the meshes, so the blend has something solid to sit over.
  fr2 <- drawLines r fr1 (srgba 1.0 0.72 0.35 0.6)
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  -- Retained line meshes: geometry uploaded once, animated by transform.
  globeMesh <- loadLineMesh r globeSegments
  gridMesh <- loadLineMesh r (gridXZ 8 1.0)
  markerMesh <- loadMesh r (sphere 1.0 2)
  lineH <- addMaterial r mid
             (withAlpha Blend (unlit (srgba 0.62 0.72 0.95 0.45)))
  markerH <- addMaterial r mid (glowing (dim 2.5 (rgb 1.0 0.62 0.25)))
  sc <- newScene
  _ <- spawn sc Nothing neutral (Just (MkDrawable gridMesh lineH))
  globe <- spawn sc Nothing (globeTransform 0.0) (Just (MkDrawable globeMesh lineH))
  m1 <- spawn sc Nothing (markerTransform 0.0 0.0) (Just (MkDrawable markerMesh markerH))
  m2 <- spawn sc Nothing (markerTransform 0.0 3.1) (Just (MkDrawable markerMesh markerH))
  fps <- newFps
  status "backend" (rendererName r)
  status "stats" (show (length globeSegments) ++ " retained segments + gizmo ribbon")
  runLoop p $ \t =>
    LIO.run (frame r p sc globe m1 m2 fps status t) >> reportFps status fps t
