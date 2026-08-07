||| What a scene needs from a graphics API, and nothing more.
|||
||| Applications are written against this alone, so they never mention WebGL,
||| WebGPU or wgpu. The rule that keeps it portable: no member may name a
||| concept that does not exist on every target -- the orrery's `canvasOf`
||| was one, and removing it is what let the native build exist.
module Offler.Gfx.Renderer

import public Control.Linear.LIO
import public Data.Linear.LMaybe

import public Offler.Gfx.Array
import Offler.Camera
import Offler.Gfx.Layout
import Offler.Light
import Offler.Material
import Offler.Math

%default covering

||| A mesh the renderer has accepted, usable only with the renderer that
||| minted it. The constructor is private; `meshHandle`/`meshIndex` exist for
||| backend modules and are not part of the application-facing API.
export
data MeshHandle : Type where
  MkMeshHandle : Int -> MeshHandle

||| For backends only: wrap the index a backend's mesh table hands back.
export %inline
meshHandle : Int -> MeshHandle
meshHandle = MkMeshHandle

||| For backends only: the index into the backend's mesh table.
export %inline
meshIndex : MeshHandle -> Int
meshIndex (MkMeshHandle i) = i

||| A renderer `r` and the frame token `f` its `beginFrame` mints.
|||
||| `f` is chosen by the implementation and determined by `r`, so each backend
||| keeps its own token type with its own private constructor: nothing outside
||| the backend can forge one. `r` stays the first argument of every method,
||| which is what lets the instance resolve -- a method mentioning only `f`
||| cannot be resolved at all.
|||
||| The token is **linear**. A pass must therefore be begun before it is drawn
||| into, ended exactly once, and never used after. Those three properties
||| would otherwise be an `IORef Bool` tested at the top of every draw, which
||| fails silently when it is wrong; here they are type errors. The token also
||| carries the per-frame object counter, so a stale count cannot be read
||| either.
public export
interface Renderer r f | r where
  ||| Shown in the status line.
  rendererName : r -> String

  ||| Upload a mesh: tightly packed position+normal vertices, in triangles.
  ||| The count travels inside `Verts`, checked against the array it came
  ||| from, so a backend can neither be told a count the buffer cannot back
  ||| nor be handed line vertices by mistake. Meshes are never freed --
  ||| create them at startup, not per frame.
  createMesh : r -> Verts Offler.Gfx.Layout.meshFloats -> IO MeshHandle

  ||| Upload the line overlay: a line list in world space, `xyz` plus one pad
  ||| float per vertex so four of them fill a single `poke16`. Consecutive
  ||| pairs are segments. Replaces whatever was there.
  setLines : r -> Verts Offler.Gfx.Layout.lineFloats -> IO ()

  ||| Drawing-buffer width over height, as it is *now*.
  aspect : r -> IO Double

  ||| Match the drawing buffer to the window, rebuilding size-dependent
  ||| resources. Safe to call when nothing has changed.
  resize : r -> IO ()

  ||| Start a frame: clear to the camera's colour, and publish the per-frame
  ||| uniforms -- projection built against the current aspect, view from the
  ||| camera's pose, the lights. `Nothing` when the surface texture could not
  ||| be acquired, which happens during a resize -- the caller then has no
  ||| token, so there is nothing it can draw into.
  beginFrame : r -> Camera -> Lights -> (time : Double) -> L1 IO (LMaybe f)

  ||| Draw one mesh with a model matrix and a material.
  draw : r -> (1 frame : f) -> MeshHandle -> Mat4 -> Material -> L1 IO f

  ||| Draw one mesh many times under a *single* bind.
  |||
  ||| Not a convenience. `L IO` is a reified monad -- `Bind` is a heap node
  ||| and `runK` interprets it -- and `runK`'s recursion is not a self tail
  ||| call, so the JavaScript backend's trampoline does not apply and the
  ||| stack grows once per bind. Threading the token through ten thousand
  ||| `draw`s puts ten thousand frames on V8's stack, which overflows between
  ||| eight and ten thousand. Looping inside one lifted `IO` action keeps the
  ||| linear discipline at the pass boundary, where it is the point, and off
  ||| the per-object path, where it is only cost.
  drawMany : r -> (1 frame : f) -> MeshHandle -> List (Mat4, Material) -> L1 IO f

  ||| Draw the whole line overlay in one call, one pixel wide and blended
  ||| over the meshes. Depth is tested but not written, so lines neither hide
  ||| each other nor stipple where they cross. Only `baseColor` (with its
  ||| alpha) is honoured; lines are always unlit.
  drawLines : r -> (1 frame : f) -> Material -> L1 IO f

  ||| Finish the frame. A no-op on WebGL2; submits the command buffer on the
  ||| two WebGPU-flavoured backends. Consumes the token.
  endFrame : r -> (1 frame : f) -> L IO ()
