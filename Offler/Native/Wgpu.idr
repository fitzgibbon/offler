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
import Data.List
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
          -> String -> Int -> String -> Int
          -> Int -> Int -> Int -> Int -> PrimIO AnyPtr

%foreign "C:offler_register_material,liboffler"
prim__register : AnyPtr -> String -> String -> Int -> Int -> PrimIO Int

%foreign "C:offler_create_instances,liboffler"
prim__createInstances : AnyPtr -> PrimIO Int

%foreign "C:offler_write_instances,liboffler"
prim__writeInstances : AnyPtr -> Int -> AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_draw_instanced,liboffler"
prim__drawInstanced : AnyPtr -> Int -> Int -> Int -> Int -> Int -> PrimIO ()

%foreign "C:offler_add_asset,liboffler"
prim__addAsset : AnyPtr -> Int -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

%foreign "C:offler_update_asset,liboffler"
prim__updateAsset : AnyPtr -> Int -> Int -> Int -> Int -> Int -> PrimIO ()

%foreign "C:offler_upload_mat_slot,liboffler"
prim__uploadMatSlot : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_upload_obj_page,liboffler"
prim__uploadObjPage : AnyPtr -> Int -> AnyPtr -> Int -> PrimIO ()

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

%foreign "C:offler_mesh_gen,liboffler"
prim__meshGen : AnyPtr -> Int -> PrimIO Int

%foreign "C:offler_free_mesh,liboffler"
prim__freeMesh : AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_set_lines,liboffler"
prim__setLines : AnyPtr -> AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_draw_lines,liboffler"
prim__drawLines : AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_begin,liboffler"
prim__begin : AnyPtr -> AnyPtr -> Double -> Double -> Double -> PrimIO Int

%foreign "C:offler_draw,liboffler"
prim__draw : AnyPtr -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_draw_slices,liboffler"
prim__drawSlices : AnyPtr -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO ()

%foreign "C:offler_end,liboffler"
prim__end : AnyPtr -> PrimIO ()

public export
record Wgpu where
  constructor MkWgpu
  ctx : AnyPtr
  globalScratch : GlobalScratch
  ||| The paged per-draw engine blocks: no frame draw ceiling.
  objScratch : Paged
  ||| One-page staging for material slots.
  matScratch : ObjScratch
  ||| Eye and forward at `beginFrame`, for sorting the transparent phase.
  eyeFwd : IORef (V3, V3)
  ||| Material assets minted so far: the next free slot of the paged
  ||| material buffers.
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
                          instanceSpec instanceStride
                          globalSize objSize objStride maxObjects)
  if prim__nullAnyPtr c /= 0
    then pure Nothing
    else do
      gs <- newGlobalScratch
      os <- newPaged
      ms <- newObjScratch
      ef <- newIORef (zero3, MkV3 0.0 0.0 (-1.0))
      ac <- newIORef (the Int 0)
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

||| Write a material value into an asset slot and upload it. The staging
||| slot is `i mod maxObjects`; the upload lands on slot `i` of the paged
||| material buffers.
fillAsset : Material m => Wgpu -> (slotIdx : Int) -> m -> IO ()
fillAsset r i v =
  case slot r.matScratch (i `mod` maxObjects) of
    Nothing => pure ()
    Just s => do
      writeMat (matWriter r.matScratch s) v
      primIO (prim__uploadMatSlot r.ctx (raw r.matScratch) (slotIndex s) i)

export
Renderer Wgpu WgpuFrame where
  rendererName _ = "wgpu"

  createMesh {t} r vs = do
    i <- primIO (prim__createMesh r.ctx (vertsRaw vs) (vertsCount vs)
                                  (topoCode t))
    g <- primIO (prim__meshGen r.ctx i)
    pure (meshHandle i g)

  createMeshIndexed r vs ix = do
    i <- primIO (prim__createMeshIndexed r.ctx (vertsRaw vs) (vertsCount vs)
                                         (indicesRaw ix) (indicesCount ix))
    g <- primIO (prim__meshGen r.ctx i)
    pure (meshHandle i g)

  freeMesh r mesh =
    primIO (prim__freeMesh r.ctx (meshIndex mesh) (meshGen mesh))

  loadTexture r src = do
    i <- case src of
           FromPath path => primIO (prim__textureFile r.ctx path)
           FromBase64 _ b64 => primIO (prim__textureB64 r.ctx b64)
    pure (textureHandle i)

  registerMaterial r {m} = do
    i <- primIO (prim__register r.ctx (materialWgsl {m}) (materialSpec {m})
                                (if matLineEntry {m} then 1 else 0)
                                (if matInstEntry {m} then 1 else 0))
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

  draw r (MkWgpuFrame i) mesh h model = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          pokeObject arr s model (handleCode h) (handleCutoff h) 0.0 0.0
          d <- if handleBlend h then depthOf r model else pure 0.0
          primIO (prim__draw r.ctx (handleAsset h) (meshIndex mesh)
                             (meshGen mesh) i
                             (if handleBlend h then 1 else 0) d)
    pure1 (MkWgpuFrame (i + 1))

  -- Chunked at page boundaries, so each foreign call's slot run shares a
  -- page (and therefore a bind group).
  drawMany r (MkWgpuFrame i) mesh h models = do
    i' <- liftIO (goChunks i models)
    pure1 (MkWgpuFrame i')
    where
      goChunks : Int -> List Mat4 -> IO Int
      goChunks i [] = pure i
      goChunks i ms@(m0 :: _) = do
        mp <- pageSlot r.objScratch i
        case mp of
          Nothing => pure i
          Just (_, arr, _) => do
            let local = i `mod` maxObjects
                (chunk, rest) = splitAt (cast (maxObjects - local)) ms
            filled <- fillModels arr local (handleCode h) (handleCutoff h) chunk
            let count = filled - local
            if count <= 0 then pure i else do
              d <- if handleBlend h then depthOf r m0 else pure 0.0
              primIO (prim__drawSlices r.ctx (handleAsset h) (meshIndex mesh)
                                       (meshGen mesh) i count
                                       (if handleBlend h then 1 else 0) d)
              goChunks (i + count) rest

  createInstances r = instanceHandle <$> primIO (prim__createInstances r.ctx)

  writeInstances r ih sl =
    primIO (prim__writeInstances r.ctx (instanceIndex ih) (instRaw sl)
                                 (instCount sl * instanceFloats)
                                 (instCount sl))

  drawInstanced r (MkWgpuFrame i) mesh h ih model = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          pokeObject arr s model (handleCode h) (handleCutoff h) 0.0 0.0
          primIO (prim__drawInstanced r.ctx (handleAsset h)
                                      (meshIndex mesh) (meshGen mesh)
                                      i (instanceIndex ih))
    pure1 (MkWgpuFrame (i + 1))

  drawGizmos r (MkWgpuFrame i) = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          -- Identity model, white lane: colours are per vertex.
          pokeObject arr s identity 1.0 1.0 1.0 1.0
          primIO (prim__drawLines r.ctx i)
    pure1 (MkWgpuFrame (i + 1))

  endFrame r (MkWgpuFrame i) = liftIO $ do
    pages <- usedPages r.objScratch i
    traverse_ (\(pg, arr, floats) =>
                 primIO (prim__uploadObjPage r.ctx pg (raw arr) floats))
              pages
    primIO (prim__end r.ctx)
