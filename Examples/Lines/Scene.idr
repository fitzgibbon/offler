||| Both gizmo modes. *Retained* (bevy 0.14's `GizmoAsset` + `Gizmo`): the
||| wireframe globe and grid are `gizmoAsset` drawables -- scene nodes spun
||| by their transforms, the globe's per-vertex latitude hues kept through
||| retention, the grid faded by its asset's tint. *Immediate*
||| (`bevy_gizmos` classic): the Lissajous ribbon rebuilds each frame with
||| a hue gradient along its length, plus an RGB axes marker from the same
||| vocabulary.
module Examples.Lines.Scene

import Data.List
import Examples.Util
import Offler.Camera
import Offler.Color
import Offler.Gizmos
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Data.Vect

import Offler.Math
import Offler.Vect
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

||| The globe's edges, in *object* space (the node's transform spins it),
||| hue by latitude: colour that survives retention.
globeSegments : GizmoData (faceCount 2 * 3)
globeSegments =
  concatV (mapV (\(a, b, c) =>
      the (Vect 3 (V3, V3, Color))
        [ seg a.position b.position
        , seg b.position c.position
        , seg c.position a.position ])
    (sphere 1.6 2))
  where
    hueAt : V3 -> Color
    hueAt v = withAlpha 0.5 (hsl (0.5 + 0.14 * v.vy / 1.6) 0.7 0.66)

    seg : V3 -> V3 -> (V3, V3, Color)
    seg a b = (a, b, hueAt a)

ribbonPoint : Double -> Double -> V3
ribbonPoint t s =
  v3 (4.2 * sin (2.0 * s + t * 0.31))
     (2.1 + 1.5 * sin (3.0 * s + t * 0.47))
     (4.2 * sin (s + t * 0.13))

||| The ribbon as immediate gizmos, hue drifting along its length: the
||| per-vertex colour the overlay draws in one call.
ribbon : Double -> GizmoData (240 * 1)
ribbon t =
  tabulateFlat 240 $ \k =>
    let s0 = cast k * (tau / 240.0)
        c = withAlpha 0.7 (hsl (0.55 + 0.25 * sin (s0 + t * 0.1)) 0.8 0.6)
     in gLine c (ribbonPoint t s0) (ribbonPoint t (s0 + tau / 240.0))

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
    -- The immediate overlay is the one thing rebuilt per frame: its
    -- geometry actually changes. Uploaded outside the pass.
    drawGizmoData r (some (ribbon t) <+> some (gAxes zero3 1.2))
  Just fr <- beginFrame r (camera t) lights t
    | Nothing => pure ()
  fr1 <- renderScene r fr sc
  -- After the meshes, so the blend has something solid to sit over.
  fr2 <- drawGizmos r fr1
  endFrame r fr2

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  gm <- registerGizmos r
  -- Retained gizmo assets: geometry frozen once, animated by transform.
  -- White tint keeps the globe's own hues; the grid fades by tint alpha.
  globeD <- gizmoAsset r gm white (some globeSegments)
  gridD <- gizmoAsset r gm (srgba 1.0 1.0 1.0 0.35) (some (gGrid white 8 1.0))
  markerMesh <- loadMesh r (sphere 1.0 2)
  markerH <- addMaterial r mid (glowing (dim 2.5 (rgb 1.0 0.62 0.25)))
  sc <- newScene
  _ <- spawn sc Nothing neutral (Just gridD)
  globe <- spawn sc Nothing (globeTransform 0.0) (Just globeD)
  m1 <- spawn sc Nothing (markerTransform 0.0 0.0) (Just (MkDrawable markerMesh markerH))
  m2 <- spawn sc Nothing (markerTransform 0.0 3.1) (Just (MkDrawable markerMesh markerH))
  fps <- newFps
  status "backend" (rendererName r)
  status "stats" (show (length globeSegments) ++ " retained segments + immediate ribbon")
  runLoop p $ \t =>
    LIO.run (frame r p sc globe m1 m2 fps status t) >> reportFps status fps t

export
app : App
app = MkApp "Lines" (soundless run)
