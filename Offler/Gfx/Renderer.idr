||| What a scene needs from a graphics API, and nothing more.
|||
||| Applications are written against this alone, so they never mention WebGL,
||| WebGPU or wgpu. The rule that keeps it portable: no member may name a
||| concept that does not exist on every target -- the orrery's `canvasOf`
||| was one, and removing it is what let the native build exist.
|||
||| Draws are phased as bevy phases them: opaque and masked draws record
||| immediately, in call order; `Blend` draws are queued and recorded at
||| `endFrame`, sorted back to front, so transparency composites over a
||| finished opaque scene whatever order the caller drew in.
module Offler.Gfx.Renderer

import public Control.Linear.LIO
import public Data.Linear.LMaybe

import public Offler.Gfx.Array
import public Offler.Gfx.Material
import Offler.Camera
import Offler.Color
import Offler.Gfx.Layout
import Offler.Light
import Offler.Math

%default covering

||| A mesh the renderer has accepted, usable only with the renderer that
||| minted it. The constructor is private; `meshHandle`/`meshIndex` exist
||| for backend modules and are not part of the application-facing API.
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
||| `f` is chosen by the implementation and determined by `r`, so each
||| backend keeps its own token type with its own private constructor:
||| nothing outside the backend can forge one. `r` stays the first argument
||| of every method, which is what lets the instance resolve.
|||
||| The token is **linear**. A pass must therefore be begun before it is
||| drawn into, ended exactly once, and never used after. Those three
||| properties would otherwise be an `IORef Bool` tested at the top of every
||| draw, which fails silently when it is wrong; here they are type errors.
||| The token also carries the per-frame slot counter, so a stale count
||| cannot be read either.
public export
interface Renderer r f | r where
  ||| Shown in the status line.
  rendererName : r -> String

  ||| Upload a mesh: tightly packed position+normal+uv vertices, in
  ||| triangles. The count travels inside `Verts`, checked against the
  ||| array it came from, so a backend can neither be told a count the
  ||| buffer cannot back nor be handed line vertices by mistake. Meshes are
  ||| never freed -- create them at startup, not per frame.
  createMesh : r -> Verts Offler.Gfx.Layout.meshFloats -> IO MeshHandle

  ||| Decode and upload an image -- PNG, JPEG, GIF and BMP at least, on
  ||| every backend. The continuation receives `Nothing` when the bytes
  ||| would not decode. Asynchronous in the browser, so it is a
  ||| continuation everywhere; that is the shape the strictest platform
  ||| forces, and the native path satisfies it trivially. Textures are
  ||| never freed -- load them at startup.
  loadTexture : r -> TextureSource -> (Maybe TextureHandle -> IO ()) -> IO ()

  ||| Build the pipelines for a material type: bevy's
  ||| `MaterialPlugin::<M>`. The generated prologue, bind group layout and
  ||| block sizes all derive from the instance; the two erased proofs are
  ||| the compile-time bounds that the material's uniform block fits its
  ||| 256-byte slot and its texture count fits the fixed bindings --
  ||| discharged by reduction at the call site, so an oversized material is
  ||| a type error there. Register each material type once, at startup.
  registerMaterial : Material m => r
                  -> {auto 0 sizeOk : FitsSlot m}
                  -> {auto 0 texOk : FitsSlots m}
                  -> IO (MaterialId m)

  ||| Upload the line overlay: a line list in world space, `xyz` plus one
  ||| pad float per vertex so four of them fill a single `poke16`.
  ||| Consecutive pairs are segments. Replaces whatever was there.
  setLines : r -> Verts Offler.Gfx.Layout.lineFloats -> IO ()

  ||| Drawing-buffer width over height, as it is *now*.
  aspect : r -> IO Double

  ||| Match the drawing buffer to the window, rebuilding size-dependent
  ||| resources. Safe to call when nothing has changed.
  resize : r -> IO ()

  ||| Start a frame: clear to the camera's colour, and publish the
  ||| per-frame uniforms -- projection built against the current aspect,
  ||| view from the camera's pose, the lights. `Nothing` when the surface
  ||| texture could not be acquired, which happens during a resize -- the
  ||| caller then has no token, so there is nothing it can draw into.
  beginFrame : r -> Camera -> Lights -> (time : Double) -> L1 IO (LMaybe f)

  ||| Draw one mesh with a model matrix and a material value. The
  ||| `MaterialId` must have been minted for `m` on this renderer, which
  ||| the phantom type holds in place: pairing it with another material
  ||| type's value is a type error.
  draw : Material m => r -> (1 frame : f)
      -> MaterialId m -> MeshHandle -> Mat4 -> m -> L1 IO f

  ||| Draw one mesh many times under a *single* bind.
  |||
  ||| Not a convenience. `L IO` is a reified monad -- `Bind` is a heap node
  ||| and `runK` interprets it -- and `runK`'s recursion is not a self tail
  ||| call, so the JavaScript backend's trampoline does not apply and the
  ||| stack grows once per bind. Threading the token through ten thousand
  ||| `draw`s puts ten thousand frames on V8's stack, which overflows
  ||| between eight and ten thousand. Looping inside one lifted `IO` action
  ||| keeps the linear discipline at the pass boundary, where it is the
  ||| point, and off the per-object path, where it is only cost.
  |||
  ||| The whole batch binds the *first* item's textures and alpha phase:
  ||| per-item uniform data varies freely, per-item bindings do not. A
  ||| batch is bevy's one-material-many-entities case.
  drawMany : Material m => r -> (1 frame : f)
          -> MaterialId m -> MeshHandle -> List (Mat4, m) -> L1 IO f

  ||| Draw the whole line overlay in one call, one pixel wide, unlit, and
  ||| blended over the meshes drawn so far. Depth is tested but not
  ||| written, so lines neither hide each other nor stipple where they
  ||| cross.
  drawLines : r -> (1 frame : f) -> Color -> L1 IO f

  ||| Finish the frame: record the sorted transparent phase, and submit.
  ||| Consumes the token.
  endFrame : r -> (1 frame : f) -> L IO ()
