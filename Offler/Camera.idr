||| The camera: a projection and a pose. The projection is described, not a
||| matrix -- the renderer knows the surface's aspect ratio and builds the
||| matrix at `beginFrame`, so a resize never leaves a stale one. Bevy splits
||| the same way (`Projection` component beside the camera's `Transform`).
module Offler.Camera

import Offler.Color
import Offler.Math
import Offler.Transform

%default total

public export
data Projection : Type where
  ||| Vertical field of view in radians, near and far planes.
  Perspective : (fovY, near, far : Double) -> Projection
  ||| Vertical half-extent of the view volume, near and far planes.
  Orthographic : (halfHeight, near, far : Double) -> Projection

public export
record Camera where
  constructor MkCamera
  projection : Projection
  transform : Transform
  ||| What the frame is cleared to, as bevy's `Camera.clear_color`.
  clearColor : Color

||| The near-black blue every example starts from.
public export
defaultClear : Color
defaultClear = rgb 0.015 0.02 0.035

||| A perspective camera at a pose. 45 degrees, the usual planes.
public export
perspectiveCamera : Transform -> Camera
perspectiveCamera t = MkCamera (Perspective 0.7854 0.1 1000.0) t defaultClear

||| An orthographic camera showing `halfHeight` world units above and below
||| centre.
public export
orthographicCamera : (halfHeight : Double) -> Transform -> Camera
orthographicCamera h t = MkCamera (Orthographic h (-1000.0) 1000.0) t defaultClear

public export
withClearColor : Color -> Camera -> Camera
withClearColor c cam = { clearColor := c } cam

||| The projection matrix, in OpenGL clip conventions; the renderer corrects z
||| for the [0,1] APIs after poking it.
public export
projMatrix : Projection -> (aspect : Double) -> Mat4
projMatrix (Perspective fovY near far) aspect = perspective fovY aspect near far
projMatrix (Orthographic h near far) aspect = orthographic h aspect near far

||| The view matrix: the inverse of the camera's pose.
public export
viewMatrix : Camera -> Mat4
viewMatrix c = viewOf c.transform

||| Where the camera sits, for specular terms.
public export
eyeOf : Camera -> V3
eyeOf c = c.transform.translation
