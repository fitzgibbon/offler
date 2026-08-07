||| `Offler.Gfx.Renderer` on wgpu-native, through the C shim in `csrc/`.
|||
||| Structurally this is `Offler/Web/Gpu.idr` with the JavaScript lambdas
||| replaced by C functions, because browser WebGPU and `webgpu.h` are the
||| same API. The uniform layout is identical: one 256-byte slot per object,
||| picked out by a dynamic bind-group offset, uploaded in a single write at
||| end of frame.
module Offler.Native.Wgpu

import Data.IORef
import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Renderer
import Offler.Gfx.Uniform
import Offler.Light
import Offler.Material
import Offler.Math

-- `Control.Linear.LIO`, re-exported by Offler.Gfx.Renderer, defines
-- `fromInteger` for its `Usage` type, which makes bare integer literals
-- ambiguous.
%hide Control.Linear.LIO.fromInteger

%default covering

||| The shim is told the layout rather than defining a second copy of it: the
||| bind group and vertex specs from `Offler.Gfx.Layout`, then the strides
||| and the slot count. Orrery's shim used to carry four `#define`s with a
||| "must match" comment.
%foreign "C:offler_init,liboffler"
prim__init : String -> String -> String -> String -> String
          -> Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign "C:offler_aspect,liboffler"
prim__aspect : AnyPtr -> PrimIO Double

%foreign "C:offler_resize,liboffler"
prim__resize : AnyPtr -> PrimIO ()

%foreign "C:offler_create_mesh,liboffler"
prim__createMesh : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO Int

%foreign "C:offler_set_lines,liboffler"
prim__setLines : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_draw_lines,liboffler"
prim__drawLines : AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_begin,liboffler"
prim__begin : AnyPtr -> AnyPtr -> Double -> Double -> Double -> PrimIO Int

%foreign "C:offler_draw,liboffler"
prim__draw : AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_draw_slices,liboffler"
prim__drawSlices : AnyPtr -> Int -> Int -> Int -> PrimIO ()

%foreign "C:offler_end,liboffler"
prim__end : AnyPtr -> AnyPtr -> Int -> PrimIO ()

public export
record Wgpu where
  constructor MkWgpu
  ctx : AnyPtr
  globalScratch : GlobalScratch
  objScratch : ObjScratch

||| The window and the device are one object on this side, so the platform
||| shares the renderer's context rather than making a second one.
export
ctxOf : Wgpu -> AnyPtr
ctxOf = ctx

||| `Nothing` when SDL or wgpu could not start, so `main` can say so plainly
||| rather than dying in the FFI.
export
initWgpu : (title : String) -> (wgsl : String) -> IO (Maybe Wgpu)
initWgpu title wgsl = do
  c <- primIO (prim__init title (wgslPrologue ++ wgsl) bindingSpec
                          meshVertexSpec lineVertexSpec
                          meshStride lineStride objStride maxObjects)
  if prim__nullAnyPtr c /= 0
    then pure Nothing
    else do
      gs <- newGlobalScratch
      os <- newObjScratch
      pure (Just (MkWgpu c gs os))

||| What would otherwise be `live : IORef Bool` and `objIndex : IORef Int` on
||| the renderer. `live` would exist because `beginFrame` can fail to acquire
||| a surface texture, and every draw would have to test it or record into a
||| pass that is not there. Here failure means no token, so there is nothing
||| to draw with.
export
data WgpuFrame : Type where
  MkWgpuFrame : (nextSlot : Int) -> WgpuFrame

||| Fill the batch's slots, then record all its draws in one foreign call.
||| Returns the next free slot, which the caller puts back in the token.
||| The filling is `pokeObjects`, whose per-object bound is a bare comparison
||| (or nothing, under `make BOUNDS=trusted`) rather than a `Maybe` per body.
drawAllWgpu : Wgpu -> Int -> Int -> List (Mat4, Material) -> IO Int
drawAllWgpu r mesh first batch = do
  filled <- pokeObjects r.objScratch first batch
  let count = filled - first
  when (count > 0) $
    primIO (prim__drawSlices r.ctx mesh first count)
  pure filled

export
Renderer Wgpu WgpuFrame where
  rendererName _ = "wgpu"

  createMesh r vs =
    meshHandle <$> primIO (prim__createMesh r.ctx (vertsRaw vs)
                                            (vertsFloats vs) (vertsCount vs))

  setLines r vs =
    primIO (prim__setLines r.ctx (vertsRaw vs) (vertsFloats vs) (vertsCount vs))

  aspect r = primIO (prim__aspect r.ctx)

  resize r = primIO (prim__resize r.ctx)

  beginFrame r cam lights t = do
    ok <- liftIO $ do
      ratio <- primIO (prim__aspect r.ctx)
      pokeGlobals r.globalScratch cam ratio lights t
      let cc = cam.clearColor
      primIO (prim__begin r.ctx (raw r.globalScratch) cc.red cc.green cc.blue)
    if ok /= 0
      then pure1 (Just (MkWgpuFrame 0))
      else pure1 Nothing

  draw r (MkWgpuFrame i) m model mat =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          pokeObject r.objScratch s model mat
          primIO (prim__draw r.ctx (meshIndex m) (slotIndex s))
        pure1 (MkWgpuFrame (i + 1))

  drawMany r (MkWgpuFrame i) m batch = do
    i' <- liftIO (drawAllWgpu r (meshIndex m) i batch)
    pure1 (MkWgpuFrame i')

  drawLines r (MkWgpuFrame i) mat =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          pokeObject r.objScratch s identity ({ unlit := True } mat)
          primIO (prim__drawLines r.ctx (slotIndex s))
        pure1 (MkWgpuFrame (i + 1))

  endFrame r (MkWgpuFrame i) =
    liftIO (primIO (prim__end r.ctx (raw r.objScratch) (i * objFloats)))
