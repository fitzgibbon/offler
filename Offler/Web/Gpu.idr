||| WebGPU implementation of `Renderer`.
|||
||| Two things differ structurally from WebGL2. Device acquisition is
||| asynchronous, so `initGpu` takes a continuation. And a render pass records
||| its commands and submits them together, so per-object uniforms cannot be
||| overwritten between draws -- each object gets its own slice of one buffer,
||| picked out by a dynamic bind-group offset, and the slices are uploaded in
||| a single write at end of frame.
module Offler.Web.Gpu

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
import Offler.Web.Js

-- `Control.Linear.LIO`, re-exported by Offler.Gfx.Renderer, defines
-- `fromInteger` for its `Usage` type, which makes bare integer literals
-- ambiguous.
%hide Control.Linear.LIO.fromInteger

%default covering

%foreign "javascript:lambda:(f)=>{(async()=>{let a=await navigator.gpu.requestAdapter();if(!a)a=await navigator.gpu.requestAdapter({forceFallbackAdapter:true});if(!a){f(null)();return}f(await a.requestDevice())()})();return 0}"
prim__requestDevice : (JSVal -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(v)=>v?1:0"
prim__isSome : JSVal -> PrimIO Int

%foreign "javascript:lambda:(dev,c)=>{const ctx=c.getContext('webgpu');ctx.configure({device:dev,format:navigator.gpu.getPreferredCanvasFormat(),alphaMode:'opaque'});return ctx}"
prim__configure : JSVal -> JSVal -> PrimIO JSVal

%foreign "javascript:lambda:(c)=>c.width/c.height"
prim__aspect : JSVal -> PrimIO Double

||| The one unavoidably long specifier: `createRenderPipeline` takes a single
||| deeply nested descriptor, and there is no way to build that incrementally
||| from Idris without a worse pile of object-poking FFI calls. Both
||| pipelines, since they share a module and a layout: the mesh pipeline from
||| the `vs` entry point, the line pipeline from `vs_line` with line-list
||| topology, blending, and no depth writes.
|||
||| The bind group layout and the vertex attributes are not written out here:
||| they are decoded from the specs `Offler.Gfx.Layout` generates, the same
||| description the shader prologue was generated from.
%foreign "javascript:lambda:(dev,src,bindSpec,meshSpec,lineSpec,meshStride,lineStride)=>{const m=dev.createShaderModule({code:src});const fmt=navigator.gpu.getPreferredCanvasFormat();const attrs=s=>s.split(';').map(e=>{const a=e.split(',').map(Number);return {shaderLocation:a[0],offset:a[1],format:a[2]>1?'float32x'+a[2]:'float32'}});const bgl=dev.createBindGroupLayout({entries:bindSpec.split(';').map(e=>{const a=e.split(',').map(Number);return {binding:a[0],visibility:a[1],buffer:{type:'uniform',hasDynamicOffset:!!a[2],minBindingSize:a[3]}}})});const layout=dev.createPipelineLayout({bindGroupLayouts:[bgl]});const pl=dev.createRenderPipeline({layout:layout,vertex:{module:m,entryPoint:'vs',buffers:[{arrayStride:meshStride,attributes:attrs(meshSpec)}]},fragment:{module:m,entryPoint:'fs',targets:[{format:fmt}]},primitive:{topology:'triangle-list',cullMode:'back'},depthStencil:{format:'depth24plus',depthWriteEnabled:true,depthCompare:'less'}});const ln=dev.createRenderPipeline({layout:layout,vertex:{module:m,entryPoint:'vs_line',buffers:[{arrayStride:lineStride,attributes:attrs(lineSpec)}]},fragment:{module:m,entryPoint:'fs',targets:[{format:fmt,blend:{color:{srcFactor:'src-alpha',dstFactor:'one-minus-src-alpha'},alpha:{srcFactor:'one',dstFactor:'one-minus-src-alpha'}}}]},primitive:{topology:'line-list'},depthStencil:{format:'depth24plus',depthWriteEnabled:false,depthCompare:'less'}});return {pl:pl,bgl:bgl,ln:ln}}"
prim__pipeline : JSVal -> String -> String -> String -> String -> Int -> Int -> PrimIO JSVal

%foreign "javascript:lambda:(dev,c)=>dev.createTexture({size:[c.width,c.height],format:'depth24plus',usage:16})"
prim__depthTexture : JSVal -> JSVal -> PrimIO JSVal

||| usage 72 = UNIFORM (64) | COPY_DST (8)
%foreign "javascript:lambda:(dev,n)=>dev.createBuffer({size:n,usage:72})"
prim__uniformBuffer : JSVal -> Int -> PrimIO JSVal

||| usage 40 = VERTEX (32) | COPY_DST (8)
%foreign "javascript:lambda:(dev,a)=>{const b=dev.createBuffer({size:a.byteLength,usage:40});dev.queue.writeBuffer(b,0,a);return b}"
prim__vertexBuffer : JSVal -> AnyPtr -> PrimIO JSVal

||| Buffers are fixed-size, so a new line overlay needs a new one and the old
||| must go. (Meshes are never destroyed; the overlay is the one buffer that
||| gets replaced.)
%foreign "javascript:lambda:(b)=>{b.destroy();return 0}"
prim__destroyBuffer : JSVal -> PrimIO Int

%foreign "javascript:lambda:(dev,p,gb,ob,sz)=>dev.createBindGroup({layout:p.bgl,entries:[{binding:0,resource:{buffer:gb}},{binding:1,resource:{buffer:ob,size:sz}}]})"
prim__bindGroup : JSVal -> JSVal -> JSVal -> JSVal -> Int -> PrimIO JSVal

%foreign "javascript:lambda:(dev,b,off,a)=>{dev.queue.writeBuffer(b,off,a);return 0}"
prim__write : JSVal -> JSVal -> Int -> AnyPtr -> PrimIO Int

||| Upload only the prefix of `a` that this frame actually filled. `count` is
||| in Float32Array elements, which is what writeBuffer takes for a typed
||| array.
%foreign "javascript:lambda:(dev,b,a,count)=>{dev.queue.writeBuffer(b,0,a,0,count);return 0}"
prim__writePrefix : JSVal -> JSVal -> AnyPtr -> Int -> PrimIO Int

%foreign "javascript:lambda:(dev,ctx,depth,pipe,r,g,b)=>{const e=dev.createCommandEncoder();const p=e.beginRenderPass({colorAttachments:[{view:ctx.getCurrentTexture().createView(),clearValue:{r:r,g:g,b:b,a:1},loadOp:'clear',storeOp:'store'}],depthStencilAttachment:{view:depth.createView(),depthClearValue:1,depthLoadOp:'clear',depthStoreOp:'store'}});p.setPipeline(pipe.pl);return {e:e,p:p}}"
prim__beginPass : JSVal -> JSVal -> JSVal -> JSVal -> Double -> Double -> Double -> PrimIO JSVal

%foreign "javascript:lambda:(pass,vb,bg,off,n)=>{pass.p.setVertexBuffer(0,vb);pass.p.setBindGroup(0,bg,[off]);pass.p.draw(n);return 0}"
prim__drawSlice : JSVal -> JSVal -> JSVal -> Int -> Int -> PrimIO Int

||| The batched form: bind the vertex buffer once, then one
||| setBindGroup+draw per already-filled slot, offsets stepping by
||| `objStride`. The loop lives on the JS side so ten thousand draws cost one
||| foreign call.
%foreign "javascript:lambda:(pass,vb,bg,first,count,stride,n)=>{pass.p.setVertexBuffer(0,vb);for(let i=0;i<count;i++){pass.p.setBindGroup(0,bg,[(first+i)*stride]);pass.p.draw(n)}return 0}"
prim__drawSlices : JSVal -> JSVal -> JSVal -> Int -> Int -> Int -> Int -> PrimIO Int

||| Swap in the line pipeline for one draw, then put the mesh pipeline back
||| so the next draw records against the state it expects.
%foreign "javascript:lambda:(pass,pipe,lb,bg,off,n)=>{pass.p.setPipeline(pipe.ln);pass.p.setVertexBuffer(0,lb);pass.p.setBindGroup(0,bg,[off]);pass.p.draw(n);pass.p.setPipeline(pipe.pl);return 0}"
prim__drawLines : JSVal -> JSVal -> JSVal -> JSVal -> Int -> Int -> PrimIO Int

%foreign "javascript:lambda:(dev,pass)=>{pass.p.end();dev.queue.submit([pass.e.finish()]);return 0}"
prim__submit : JSVal -> JSVal -> PrimIO Int

public export
record Gpu where
  constructor MkGpu
  device, ctx, canvas, pipeline, globalBuf, objBuf, bindGroup : JSVal
  ||| The mesh table: a `MeshHandle` indexes it.
  meshes : JSVal
  globalScratch : GlobalScratch
  objScratch : ObjScratch
  ||| Rebuilt whenever the canvas changes size, so it always matches.
  depth : IORef JSVal
  ||| Empty until the first overlay arrives. Never a placeholder: a stand-in
  ||| device here is what once let a `setLines` destroy the device instead of
  ||| a buffer.
  lineBuf : IORef (Maybe JSVal)
  lineCount : IORef Int

||| Acquire a device and build the pipelines. Calls back with `Nothing` when
||| no adapter is available, so the page can say so rather than hanging.
export
initGpu : (canvasId : String) -> (wgsl : String) -> (Maybe Gpu -> IO ()) -> IO ()
initGpu canvasId wgsl k =
  ignore (primIO (prim__requestDevice (\dev => toPrim (withDevice dev >> pure 0))))
  where
    withDevice : JSVal -> IO ()
    withDevice dev = do
      ok <- primIO (prim__isSome dev)
      if ok == 0 then k Nothing else do
        canvas <- byId canvasId
        ignore (syncSize canvas)
        ctx <- primIO (prim__configure dev canvas)
        -- The declarations the offsets and the pipeline layout were both
        -- derived from, ahead of the authored body.
        pipe <- primIO (prim__pipeline dev (wgslPrologue ++ wgsl) bindingSpec
                                       meshVertexSpec lineVertexSpec
                                       meshStride lineStride)
        depthTex <- primIO (prim__depthTexture dev canvas)
        depth <- newIORef depthTex
        gbuf <- primIO (prim__uniformBuffer dev globalSize)
        obuf <- primIO (prim__uniformBuffer dev (objStride * maxObjects))
        bg <- primIO (prim__bindGroup dev pipe gbuf obuf objSize)
        gs <- newGlobalScratch
        -- One slot per object, filled during the frame and uploaded in a
        -- single writeBuffer at the end. Writing per object means a queue
        -- call per draw, which is what makes WebGPU far slower than WebGL2
        -- at scale.
        os <- newObjScratch
        meshes <- newStore
        lb <- newIORef Prelude.Nothing
        lc <- newIORef 0
        k (Just (MkGpu dev ctx canvas pipe gbuf obuf bg meshes gs os depth lb lc))

||| The pass and the object counter, which would otherwise be two `IORef`s on
||| the renderer read at the top of every draw. Holding them in the linear
||| token means a draw cannot see a pass that has ended, and cannot read a
||| slot index left over from the previous frame: there is no token to read
||| them from.
export
data GpuFrame : Type where
  MkGpuFrame : (pass : JSVal) -> (nextSlot : Int) -> GpuFrame

||| Fill one object slot and record its draw. Plain `IO`, so the batched form
||| can loop over it without a bind per object. Takes a `Slot`, so it cannot
||| be reached with an index nobody checked.
drawOneGpu : Gpu -> JSVal -> JSVal -> Int -> Slot -> Mat4 -> Material -> IO ()
drawOneGpu r p vb n s model mat = do
  pokeObject r.objScratch s model mat
  ignore (primIO (prim__drawSlice p vb r.bindGroup (slotOffset s) n))

||| Fill the batch's slots, then record all its draws in one foreign call.
||| Returns the next free slot, which the caller puts back in the token.
||| The filling is `pokeObjects`, whose per-object bound is a bare comparison
||| (or nothing, under `make BOUNDS=trusted`) rather than a `Maybe` per body.
drawAllGpu : Gpu -> JSVal -> JSVal -> Int -> Int -> List (Mat4, Material) -> IO Int
drawAllGpu r p vb n first batch = do
  filled <- pokeObjects r.objScratch first batch
  let count = filled - first
  when (count > 0) $
    ignore (primIO (prim__drawSlices p vb r.bindGroup first count objStride n))
  pure filled

export
Renderer Gpu GpuFrame where
  rendererName _ = "WebGPU"

  createMesh r vs = do
    buf <- primIO (prim__vertexBuffer r.device (vertsRaw vs))
    meshHandle <$> storeAdd r.meshes buf (vertsCount vs)

  setLines r vs = do
    old <- readIORef r.lineBuf
    traverse_ (\b => ignore (primIO (prim__destroyBuffer b))) old
    buf <- primIO (prim__vertexBuffer r.device (vertsRaw vs))
    writeIORef r.lineBuf (Just buf)
    writeIORef r.lineCount (vertsCount vs)

  aspect r = primIO (prim__aspect r.canvas)

  -- getCurrentTexture() follows the canvas automatically, but the depth
  -- attachment does not: it must be the same size or the pass is invalid.
  resize r = do
    changed <- syncSize r.canvas
    when changed $ do
      tex <- primIO (prim__depthTexture r.device r.canvas)
      writeIORef r.depth tex

  beginFrame r cam lights t = do
    p <- liftIO $ do
      ratio <- primIO (prim__aspect r.canvas)
      pokeGlobals r.globalScratch cam ratio lights t
      ignore (primIO (prim__write r.device r.globalBuf 0 (raw r.globalScratch)))
      depthTex <- readIORef r.depth
      let cc = cam.clearColor
      primIO (prim__beginPass r.device r.ctx depthTex r.pipeline
                              cc.red cc.green cc.blue)
    pure1 (Just (MkGpuFrame p 0))

  draw r (MkGpuFrame p i) m model mat =
    case slot r.objScratch i of
      Nothing => pure1 (MkGpuFrame p i)
      Just s => do
        liftIO $ do
          vb <- storeBuf r.meshes (meshIndex m)
          n <- storeCount r.meshes (meshIndex m)
          drawOneGpu r p vb n s model mat
        pure1 (MkGpuFrame p (i + 1))

  drawMany r (MkGpuFrame p i) m batch = do
    i' <- liftIO $ do
      vb <- storeBuf r.meshes (meshIndex m)
      n <- storeCount r.meshes (meshIndex m)
      drawAllGpu r p vb n i batch
    pure1 (MkGpuFrame p i')

  drawLines r (MkGpuFrame p i) mat = do
    st <- liftIO $ do
      n <- readIORef r.lineCount
      lb <- readIORef r.lineBuf
      pure (n, lb)
    case st of
      (n, Just lb) => case slot r.objScratch i of
        Just s =>
          if n <= 0
            then pure1 (MkGpuFrame p i)
            else do
              liftIO $ do
                pokeObject r.objScratch s identity ({ unlit := True } mat)
                ignore (primIO (prim__drawLines p r.pipeline lb r.bindGroup
                                                (slotOffset s) n))
              pure1 (MkGpuFrame p (i + 1))
        Nothing => pure1 (MkGpuFrame p i)
      _ => pure1 (MkGpuFrame p i)

  endFrame r (MkGpuFrame p i) = liftIO $ do
    -- One upload for every object. Queue writes are ordered before the
    -- submit that follows, so the slices are in place when the pass executes.
    ignore (primIO (prim__writePrefix r.device r.objBuf (raw r.objScratch) (i * objFloats)))
    ignore (primIO (prim__submit r.device p))
