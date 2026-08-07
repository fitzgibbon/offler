||| What a scene needs from a graphics API, and nothing more.
|||
||| Applications are written against this alone, so they never mention WebGL,
||| WebGPU or wgpu. The rule that keeps it portable: no member may name a
||| concept that does not exist on every target -- the orrery's `canvasOf`
||| was one, and removing it is what let the native build exist.
|||
||| The model is retained, as bevy's is. Meshes, textures and material
||| *assets* are created once and referenced by typed handles; a draw is a
||| mesh handle, a material handle and a model matrix, and costs no
||| material work at all -- the asset's uniform block went to the GPU when
||| the asset was made. Draws are phased as bevy phases them: opaque and
||| masked record in call order; `Blend` draws are queued and recorded at
||| `endFrame`, sorted back to front.
module Offler.Gfx.Renderer

import public Control.Linear.LIO
import public Data.Linear.LMaybe

import public Offler.Gfx.Array
import public Offler.Gfx.Instances
import public Offler.Gfx.Material
import Offler.Camera
import Offler.Color
import Offler.Gfx.Layout
import Offler.Light
import Offler.Math

%default covering

||| A mesh the renderer has accepted, usable only with the renderer that
||| minted it, indexed by its primitive topology -- so the pipeline variant
||| a draw needs is decided by the type checker, not by a runtime mesh key.
||| The constructor is private; `meshHandle`/`meshIndex`/`meshGen` exist for
||| backend modules and are not part of the application-facing API.
|||
||| The handle carries a *generation* beside the table index. `freeMesh`
||| bumps the entry's generation and recycles the index, so the table never
||| grows past the peak live mesh count; a draw checks the handle's
||| generation against the entry's and a stale handle -- one whose index has
||| been recycled -- draws nothing rather than someone else's mesh.
export
data MeshHandle : Topology -> Type where
  MkMeshHandle : (idx : Int) -> (gen : Int) -> MeshHandle t

||| For backends only: wrap the index and generation a backend's mesh table
||| hands back.
export %inline
meshHandle : (idx : Int) -> (gen : Int) -> MeshHandle t
meshHandle = MkMeshHandle

||| For backends only: the index into the backend's mesh table.
export %inline
meshIndex : MeshHandle t -> Int
meshIndex (MkMeshHandle i _) = i

||| For backends only: the generation the index was minted at.
export %inline
meshGen : MeshHandle t -> Int
meshGen (MkMeshHandle _ g) = g

||| A GPU-side instance buffer the renderer owns: bevy's per-batch
||| instance buffer as a first-class handle. Create once, `writeInstances`
||| whenever the crowd moves, `drawInstanced` to draw the whole batch in
||| one call.
export
data InstanceHandle : Type where
  MkInstanceHandle : Int -> InstanceHandle

||| For backends only.
export %inline
instanceHandle : Int -> InstanceHandle
instanceHandle = MkInstanceHandle

export %inline
instanceIndex : InstanceHandle -> Int
instanceIndex (MkInstanceHandle i) = i

||| A renderer `r` and the frame token `f` its `beginFrame` mints.
|||
||| `f` is chosen by the implementation and determined by `r`, so each
||| backend keeps its own token type with its own private constructor:
||| nothing outside the backend can forge one.
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

  ||| Upload a mesh of a topology: tightly packed vertices in that
  ||| topology's layout (position+normal+uv for triangles, padded positions
  ||| for lines). The count travels inside `Verts`, checked against the
  ||| array it came from. Create meshes at load time, not per frame;
  ||| release one that will not draw again with `freeMesh`.
  createMesh : {t : Topology} -> r -> Verts t -> IO (MeshHandle t)

  ||| Release a mesh's GPU buffers and recycle its table index. The handle
  ||| and any copies of it are stale afterwards: their generation no longer
  ||| matches the entry's, so draws against them are silent no-ops -- even
  ||| after the index is reused for a new mesh. Unrestricted handles cannot
  ||| make use-after-free a type error the way the frame token does -- a
  ||| linear handle could not be drawn twice -- so this is the honest
  ||| runtime seam: bevy frees on refcount for the same reason.
  freeMesh : r -> MeshHandle t -> IO ()

  ||| A triangle mesh with an index list: shared vertices are stored once
  ||| and named many times, which is how most meshes want to exist.
  createMeshIndexed : r -> Verts Triangles -> Indices -> IO (MeshHandle Triangles)

  ||| Decode and upload an image -- PNG, JPEG, GIF and BMP at least, on
  ||| every backend. The handle returns *immediately* and is valid to draw
  ||| with at once: it reads as 1x1 white until the decode lands (the next
  ||| frame, typically), and stays white if the bytes never decode --
  ||| bevy's handle-before-loaded behaviour. Textures are never freed.
  loadTexture : r -> TextureSource -> IO TextureHandle

  ||| Build the pipelines for a material type: bevy's
  ||| `MaterialPlugin::<M>`. The generated prologue, bind group layout and
  ||| block sizes all derive from the instance; the two erased proofs are
  ||| the compile-time bounds that the material's uniform block fits its
  ||| 256-byte slot and its texture count fits the fixed bindings.
  ||| Pipelines are built per topology and alpha phase; the line variants
  ||| exist only when the material declares `matLineEntry`. Register each
  ||| material type once, at startup.
  registerMaterial : Material m => r
                  -> {auto 0 sizeOk : FitsSlot m}
                  -> {auto 0 texOk : FitsSlots m}
                  -> IO (MaterialId m)

  ||| Create a material *asset* from a value: writes its uniform block into
  ||| the asset's own 256-byte slot and records its textures and alpha
  ||| phase. Drawing with the returned handle does no material work per
  ||| draw. Assets are never freed; there are `maxObjects` slots.
  addMaterial : Material m => r -> MaterialId m -> m -> IO (Handle m)

  ||| Re-fill an existing asset's slot and bindings from a new value,
  ||| minting a fresh handle for it (the alpha lane rides in the handle, so
  ||| handles are immutable). The old handle keeps drawing with the
  ||| updated data but a stale alpha phase; prefer the returned one.
  updateMaterial : Material m => r -> Handle m -> m -> IO (Handle m)

  ||| Upload the gizmo overlay: an immediate-mode coloured line list in
  ||| world space. One overlay per renderer, replaced wholesale -- the
  ||| *replaceable* GPU buffer is why this is an engine primitive at all
  ||| (meshes are never freed, so a per-frame `createMesh` would leak).
  ||| Applications should not call this directly: `Offler.Gizmos` is the
  ||| vocabulary over it, and retained gizmos are ordinary `Lines` meshes.
  setGizmos : r -> Verts Lines -> IO ()

  ||| Drawing-buffer width over height, as it is *now*.
  aspect : r -> IO Double

  ||| Start a frame: clear to the camera's colour, and publish the
  ||| per-frame uniforms -- projection built against the current aspect,
  ||| view from the camera's pose, up to `maxLights` directional lights.
  ||| Also where the drawing surface is re-synced to the window, so
  ||| applications never handle resizes themselves. `Nothing` when the
  ||| surface texture could not be acquired -- the caller then has no
  ||| token, so there is nothing it can draw into.
  beginFrame : r -> Camera -> Lights -> (time : Double) -> L1 IO (LMaybe f)

  ||| Draw a mesh with a material asset and a model matrix. The handle must
  ||| have been minted for `m` on this renderer (phantom-typed), and the
  ||| mesh's topology must be one the material's shaders support --
  ||| `TopoOk`, proved at compile time.
  draw : Material m => r -> (1 frame : f)
      -> MeshHandle t -> Handle m -> Mat4
      -> {auto 0 ok : TopoOk t m} -> L1 IO f

  ||| Draw one mesh many times with one material asset: model matrices
  ||| only, since the material data is retained. Loops inside a single
  ||| lifted IO action, so ten thousand draws cost a handful of linear
  ||| binds rather than a stack frame each -- `runK`'s recursion is not a
  ||| self tail call, and V8 overflows between eight and ten thousand of
  ||| them otherwise.
  drawMany : Material m => r -> (1 frame : f)
          -> MeshHandle t -> Handle m -> List Mat4
          -> {auto 0 ok : TopoOk t m} -> L1 IO f

  ||| Draw the whole gizmo overlay in one call: per-vertex coloured, one
  ||| pixel wide, unlit, blended over the meshes drawn so far. Depth is
  ||| tested but not written, so lines neither hide each other nor stipple
  ||| where they cross.
  drawGizmos : r -> (1 frame : f) -> L1 IO f

  ||| A GPU-side instance buffer, grown on write. Create once per crowd.
  createInstances : r -> IO InstanceHandle

  ||| Replace the buffer's contents with a filled slice and remember its
  ||| count: one upload per change, typically per frame.
  writeInstances : r -> InstanceHandle -> InstSlice -> IO ()

  ||| Draw one triangle mesh once *per instance in the buffer*, in a single
  ||| call: mesh attributes at vertex rate, the instance matrix and colour
  ||| at instance rate, `Mat4` the whole batch's transform (`o.model`).
  ||| The material must declare its instanced entry -- `InstOk`, proved at
  ||| compile time, offler's spelling of bevy's automatic batching.
  ||| Instanced draws run in the opaque phase: a batch cannot be
  ||| depth-sorted within itself, which is bevy's limitation too.
  drawInstanced : Material m => r -> (1 frame : f)
               -> MeshHandle Triangles -> Handle m -> InstanceHandle
               -> Mat4 -> {auto 0 inst : InstOk m} -> L1 IO f

  ||| Finish the frame: record the sorted transparent phase, and submit.
  ||| Consumes the token.
  endFrame : r -> (1 frame : f) -> L IO ()

||| A mesh paired with a material asset that can draw it, topology
||| compatibility proved at construction and erased -- what a scene node
||| holds. The existential keeps `Scene` monomorphic while nodes mix
||| material types freely, and the proof cannot be skipped: there is no
||| other way to build one.
public export
data Drawable : Type where
  MkDrawable : Material m => {0 t : Topology}
            -> MeshHandle t -> Handle m
            -> {auto 0 ok : TopoOk t m} -> Drawable

||| Draw a flattened scene: a list of world transforms and drawables, as
||| `Offler.Scene.collect` produces. One linear bind per node -- scene
||| granularity, not crowd granularity; crowds want `drawMany`.
export
drawAll : Renderer r f => r -> (1 frame : f) -> List (Mat4, Drawable) -> L1 IO f
drawAll r fr [] = pure1 fr
drawAll r fr ((model, MkDrawable mesh mat) :: rest) = do
  fr' <- draw r fr mesh mat model
  drawAll r fr' rest
