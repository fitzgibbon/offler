||| `Offler.Gfx.Renderer` on wgpu-native, through the C shim in `csrc/`.
|||
||| Structurally this is `Offler/Web/Gpu.idr` with the JavaScript lambdas
||| replaced by C functions, because browser WebGPU and `webgpu.h` are the
||| same API: per-material pipeline pairs built from the same generated WGSL
||| and specs, the same pair of dynamic offsets picking a draw's object and
||| material slots, the same cached bind groups per texture set, and the
||| same sorted transparent phase at `endFrame`.
module Offler.Native.Wgpu

import Data.IORef
import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Material
import Offler.Gfx.Renderer
import Offler.Gfx.Uniform
import Offler.Light
import Offler.Math
import Offler.Shaders
import Offler.Transform

-- `Control.Linear.LIO`, re-exported by Offler.Gfx.Renderer, defines
-- `fromInteger` for its `Usage` type, which makes bare integer literals
-- ambiguous.
%hide Control.Linear.LIO.fromInteger

%default covering

||| The shim is told the layout rather than defining a second copy of it:
||| the line pipeline's generated WGSL and specs, the mesh vertex spec, and
||| the proved sizes.
%foreign "C:offler_init,liboffler"
prim__init : String -> String -> String -> String -> Int
          -> String -> Int -> Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign "C:offler_register_material,liboffler"
prim__register : AnyPtr -> String -> String -> PrimIO Int

%foreign "C:offler_texture_file,liboffler"
prim__textureFile : AnyPtr -> String -> PrimIO Int

%foreign "C:offler_texture_b64,liboffler"
prim__textureB64 : AnyPtr -> String -> PrimIO Int

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
prim__draw : AnyPtr -> Int -> Int -> Int
          -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_draw_slices,liboffler"
prim__drawSlices : AnyPtr -> Int -> Int -> Int -> Int
                -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_end,liboffler"
prim__end : AnyPtr -> AnyPtr -> AnyPtr -> Int -> PrimIO ()

public export
record Wgpu where
  constructor MkWgpu
  ctx : AnyPtr
  globalScratch : GlobalScratch
  objScratch, matScratch : ObjScratch
  ||| Eye and forward at `beginFrame`, for sorting the transparent phase.
  eyeFwd : IORef (V3, V3)

||| The window and the device are one object on this side, so the platform
||| shares the renderer's context rather than making a second one.
export
ctxOf : Wgpu -> AnyPtr
ctxOf = ctx

||| `Nothing` when SDL or wgpu could not start, so `main` can say so plainly
||| rather than dying in the FFI. Materials arrive later, through
||| `registerMaterial`.
export
initWgpu : (title : String) -> IO (Maybe Wgpu)
initWgpu title = do
  c <- primIO (prim__init title (wgslLinePrologue ++ lineWgslSrc)
                          lineBindSpec lineVertexSpec lineStride
                          meshVertexSpec meshStride
                          globalSize objSize objStride maxObjects)
  if prim__nullAnyPtr c /= 0
    then pure Nothing
    else do
      gs <- newGlobalScratch
      os <- newObjScratch
      ms <- newObjScratch
      ef <- newIORef (zero3, MkV3 0.0 0.0 (-1.0))
      pure (Just (MkWgpu c gs os ms ef))

||| The slot counter, held in the linear token: failure to acquire a surface
||| means no token, so there is nothing to draw with.
export
data WgpuFrame : Type where
  MkWgpuFrame : (nextSlot : Int) -> WgpuFrame

||| View-space depth of a model's translation, for the transparent sort.
depthOf : Wgpu -> Mat4 -> IO Double
depthOf r model = do
  (eye, fwd) <- readIORef r.eyeFwd
  pure (dot3 fwd (sub3 (MkV3 model.m12 model.m13 model.m14) eye))

export
Renderer Wgpu WgpuFrame where
  rendererName _ = "wgpu"

  createMesh r vs =
    meshHandle <$> primIO (prim__createMesh r.ctx (vertsRaw vs)
                                            (vertsFloats vs) (vertsCount vs))

  loadTexture r src k = do
    i <- case src of
           FromPath path => primIO (prim__textureFile r.ctx path)
           FromBase64 _ b64 => primIO (prim__textureB64 r.ctx b64)
    k (if i < 0 then Nothing else Just (textureHandle i))

  registerMaterial r {m} = do
    i <- primIO (prim__register r.ctx (materialWgsl {m}) (materialSpec {m}))
    pure (materialId i)

  setLines r vs =
    primIO (prim__setLines r.ctx (vertsRaw vs) (vertsFloats vs) (vertsCount vs))

  aspect r = primIO (prim__aspect r.ctx)

  resize r = primIO (prim__resize r.ctx)

  beginFrame r cam lights t = do
    ok <- liftIO $ do
      ratio <- primIO (prim__aspect r.ctx)
      pokeGlobals r.globalScratch cam ratio lights t True
      writeIORef r.eyeFwd
        (eyeOf cam, qRotate cam.transform.rotation (MkV3 0.0 0.0 (-1.0)))
      let cc = cam.clearColor
      primIO (prim__begin r.ctx (raw r.globalScratch) cc.red cc.green cc.blue)
    if ok /= 0
      then pure1 (Just (MkWgpuFrame 0))
      else pure1 Nothing

  draw r (MkWgpuFrame i) mid mesh model v =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          let am = alphaMode v
          pokeObject r.objScratch s model (alphaCode am) (alphaCutoff am) 0.0 0.0
          writeMat (matWriter r.matScratch s) v
          let (t0, t1, t2, t3) = texIds v
          d <- if isBlend am then depthOf r model else pure 0.0
          primIO (prim__draw r.ctx (materialIdIndex mid) (meshIndex mesh)
                             (slotIndex s) t0 t1 t2 t3
                             (if isBlend am then 1 else 0) d)
        pure1 (MkWgpuFrame (i + 1))

  drawMany r (MkWgpuFrame i) mid mesh batch = do
    i' <- liftIO $ case batch of
      [] => pure i
      ((mdl0, v0) :: _) => do
        let am = alphaMode v0
            (t0, t1, t2, t3) = texIds v0
        filled <- fillBatch r.objScratch r.matScratch i batch
        let count = filled - i
        when (count > 0) $ do
          d <- if isBlend am then depthOf r mdl0 else pure 0.0
          primIO (prim__drawSlices r.ctx (materialIdIndex mid) (meshIndex mesh)
                                   i count t0 t1 t2 t3
                                   (if isBlend am then 1 else 0) d)
        pure filled
    pure1 (MkWgpuFrame i')

  drawLines r (MkWgpuFrame i) colour =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          pokeObject r.objScratch s identity
                     colour.red colour.green colour.blue colour.alpha
          primIO (prim__drawLines r.ctx (slotIndex s))
        pure1 (MkWgpuFrame (i + 1))

  endFrame r (MkWgpuFrame i) =
    liftIO (primIO (prim__end r.ctx (raw r.objScratch) (raw r.matScratch)
                              (i * objFloats)))
