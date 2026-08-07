||| `Offler.Gfx.Renderer` on wgpu-native, through the C shim in `csrc/`.
|||
||| Structurally this is `Offler/Web/Gpu.idr` with the JavaScript lambdas
||| replaced by C functions, because browser WebGPU and `webgpu.h` are the
||| same API: retained material assets with their slot, textures and cached
||| bind group; pipeline variants per topology and alpha phase; the same
||| pair of dynamic offsets pairing a draw's object slot with its asset's
||| material slot; and the same sorted transparent phase at `endFrame`.
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
||| the gizmo pipeline's generated WGSL and specs, both vertex specs, and
||| the proved sizes.
%foreign "C:offler_init,liboffler"
prim__init : String -> String -> String -> String -> Int
          -> String -> Int -> Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign "C:offler_register_material,liboffler"
prim__register : AnyPtr -> String -> String -> Int -> PrimIO Int

%foreign "C:offler_add_asset,liboffler"
prim__addAsset : AnyPtr -> Int -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

%foreign "C:offler_update_asset,liboffler"
prim__updateAsset : AnyPtr -> Int -> Int -> Int -> Int -> Int -> PrimIO ()

%foreign "C:offler_upload_mat_slot,liboffler"
prim__uploadMatSlot : AnyPtr -> AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_texture_file,liboffler"
prim__textureFile : AnyPtr -> String -> PrimIO Int

%foreign "C:offler_texture_b64,liboffler"
prim__textureB64 : AnyPtr -> String -> PrimIO Int

%foreign "C:offler_aspect,liboffler"
prim__aspect : AnyPtr -> PrimIO Double

%foreign "C:offler_create_mesh,liboffler"
prim__createMesh : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO Int

%foreign "C:offler_create_mesh_indexed,liboffler"
prim__createMeshIndexed : AnyPtr -> AnyPtr -> Int -> AnyPtr -> Int -> PrimIO Int

%foreign "C:offler_free_mesh,liboffler"
prim__freeMesh : AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_set_lines,liboffler"
prim__setLines : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_draw_lines,liboffler"
prim__drawLines : AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_begin,liboffler"
prim__begin : AnyPtr -> AnyPtr -> Double -> Double -> Double -> PrimIO Int

%foreign "C:offler_draw,liboffler"
prim__draw : AnyPtr -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_draw_slices,liboffler"
prim__drawSlices : AnyPtr -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_end,liboffler"
prim__end : AnyPtr -> AnyPtr -> Int -> PrimIO ()

public export
record Wgpu where
  constructor MkWgpu
  ctx : AnyPtr
  globalScratch : GlobalScratch
  objScratch, matScratch : ObjScratch
  ||| Eye and forward at `beginFrame`, for sorting the transparent phase.
  eyeFwd : IORef (V3, V3)
  ||| Material assets minted so far: the next free slot of the material
  ||| buffer.
  assetCount : IORef Int

||| The window and the device are one object on this side, so the platform
||| shares the renderer's context rather than making a second one.
export
ctxOf : Wgpu -> AnyPtr
ctxOf = ctx

||| `Nothing` when SDL or wgpu could not start, so `main` can say so plainly
||| rather than dying in the FFI. Materials arrive later, through
||| `registerMaterial`/`addMaterial`.
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
      ac <- newIORef 0
      pure (Just (MkWgpu c gs os ms ef ac))

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

||| Write a material value into an asset slot and upload it.
fillAsset : Material m => Wgpu -> (slotIdx : Int) -> m -> IO ()
fillAsset r i v =
  case slot r.matScratch i of
    Nothing => pure ()
    Just s => do
      writeMat (matWriter r.matScratch s) v
      primIO (prim__uploadMatSlot r.ctx (raw r.matScratch) (slotIndex s))

export
Renderer Wgpu WgpuFrame where
  rendererName _ = "wgpu"

  createMesh {t} r vs =
    meshHandle <$> primIO (prim__createMesh r.ctx (vertsRaw vs) (vertsCount vs)
                                            (topoCode t))

  createMeshIndexed r vs ix =
    meshHandle <$> primIO (prim__createMeshIndexed r.ctx (vertsRaw vs)
                                                   (vertsCount vs)
                                                   (indicesRaw ix)
                                                   (indicesCount ix))

  freeMesh r mesh = primIO (prim__freeMesh r.ctx (meshIndex mesh))

  loadTexture r src = do
    i <- case src of
           FromPath path => primIO (prim__textureFile r.ctx path)
           FromBase64 _ b64 => primIO (prim__textureB64 r.ctx b64)
    pure (textureHandle i)

  registerMaterial r {m} = do
    i <- primIO (prim__register r.ctx (materialWgsl {m}) (materialSpec {m})
                                (if matLineEntry {m} then 1 else 0))
    pure (materialId i)

  addMaterial r mid v = do
    i <- readIORef r.assetCount
    writeIORef r.assetCount (i + 1)
    fillAsset r i v
    let (t0, t1, t2, t3) = texIds v
    a <- primIO (prim__addAsset r.ctx (materialIdIndex mid) i t0 t1 t2 t3)
    pure (handleFor a (alphaMode v))

  updateMaterial r h v = do
    let a = handleAsset h
    fillAsset r a v
    let (t0, t1, t2, t3) = texIds v
    primIO (prim__updateAsset r.ctx a t0 t1 t2 t3)
    pure (handleFor a (alphaMode v))

  setGizmos r vs =
    primIO (prim__setLines r.ctx (vertsRaw vs) (vertsFloats vs) (vertsCount vs))

  aspect r = primIO (prim__aspect r.ctx)

  -- The shim absorbs resizes at frame start: the surface configuration and
  -- the depth attachment follow the window together.
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

  draw r (MkWgpuFrame i) mesh h model =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          pokeObject r.objScratch s model (handleCode h) (handleCutoff h) 0.0 0.0
          d <- if handleBlend h then depthOf r model else pure 0.0
          primIO (prim__draw r.ctx (handleAsset h) (meshIndex mesh)
                             (slotIndex s) (if handleBlend h then 1 else 0) d)
        pure1 (MkWgpuFrame (i + 1))

  drawMany r (MkWgpuFrame i) mesh h models = do
    i' <- liftIO $ case models of
      [] => pure i
      (mdl0 :: _) => do
        filled <- fillModels r.objScratch i (handleCode h) (handleCutoff h) models
        let count = filled - i
        when (count > 0) $ do
          d <- if handleBlend h then depthOf r mdl0 else pure 0.0
          primIO (prim__drawSlices r.ctx (handleAsset h) (meshIndex mesh)
                                   i count (if handleBlend h then 1 else 0) d)
        pure filled
    pure1 (MkWgpuFrame i')

  drawGizmos r (MkWgpuFrame i) =
    case slot r.objScratch i of
      Nothing => pure1 (MkWgpuFrame i)
      Just s => do
        liftIO $ do
          -- Identity model, white lane: colours are per vertex.
          pokeObject r.objScratch s identity 1.0 1.0 1.0 1.0
          primIO (prim__drawLines r.ctx (slotIndex s))
        pure1 (MkWgpuFrame (i + 1))

  endFrame r (MkWgpuFrame i) =
    liftIO (primIO (prim__end r.ctx (raw r.objScratch) (i * objFloats)))
