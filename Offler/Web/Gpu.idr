||| WebGPU implementation of `Renderer`.
|||
||| Retained, as the interface demands: material *assets* own a slot of the
||| material buffer and a cached bind group, both made at `addMaterial`, so
||| a draw records only the engine block (model matrix, alpha lane) and one
||| foreign call. Pipelines are built per material type at registration --
||| triangle and (when declared) line topology, each in opaque and blended
||| variants -- and picked by the mesh's topology at draw. `Blend` draws
||| queue JS-side and record at `endFrame`, sorted back to front.
module Offler.Web.Gpu

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

||| The mutable JS runtime: mesh, texture, material and asset tables, the
||| pending transparent phase, the shared sampler and 1x1 white, the
||| bind-group cache `bgFor`, and the draw executor `exec` -- defined once
||| here because each `%foreign` lambda is otherwise its own world.
|||
||| Texture usage bits differ from buffer usage bits: COPY_DST is 2 for
||| textures (8 for buffers); white is TEXTURE_BINDING(4)|COPY_DST(2) = 6.
||| `texGen` counts decode completions: asset bind groups rebuild lazily
||| when it moves, which is how a texture that decodes after the assets
||| referencing it were made still reaches them.
%foreign "javascript:lambda:(dev,gbuf,obuf,mbuf,objSize)=>{const rt={dev:dev,gbuf:gbuf,obuf:obuf,mbuf:mbuf,objSize:objSize,meshes:[],texs:[],texGen:0,mats:[],assets:[],pend:[],lineBuf:null,lineCount:0};dev.addEventListener('uncapturederror',(e)=>{const el=document.getElementById('error');if(el){el.style.display='block';el.textContent='WebGPU: '+e.error.message}});rt.sampler=dev.createSampler({magFilter:'linear',minFilter:'linear',addressModeU:'repeat',addressModeV:'repeat'});const w=dev.createTexture({size:[1,1],format:'rgba8unorm',usage:6});dev.queue.writeTexture({texture:w},new Uint8Array([255,255,255,255]),{},[1,1]);rt.white=w.createView();rt.bgFor=(M,t0,t1,t2,t3)=>{const key=rt.texGen+':'+t0+','+t1+','+t2+','+t3;let bg=M.bgs[key];if(!bg){const es=[{binding:0,resource:{buffer:rt.gbuf}},{binding:1,resource:{buffer:rt.obuf,size:rt.objSize}},{binding:2,resource:{buffer:rt.mbuf,size:M.matSize}}];const ts=[t0,t1,t2,t3];for(let i=0;i<M.texCount;i++){es.push({binding:3+2*i,resource:ts[i]>=0&&rt.texs[ts[i]]?rt.texs[ts[i]]:rt.white});es.push({binding:4+2*i,resource:rt.sampler})}bg=rt.dev.createBindGroup({layout:M.pl.getBindGroupLayout(0),entries:es});M.bgs[key]=bg}return bg};rt.exec=(pass,ai,mi,slot,blend)=>{const A=rt.assets[ai];const M=rt.mats[A.mat];const mm=rt.meshes[mi];if(A.gen!==rt.texGen){A.bg=rt.bgFor(M,A.t0,A.t1,A.t2,A.t3);A.gen=rt.texGen}const pipe=mm.topo===1?(blend?M.lplb:M.lpl):(blend?M.plb:M.pl);if(!pipe)return;pass.p.setPipeline(pipe);pass.p.setVertexBuffer(0,mm.b);pass.p.setBindGroup(0,A.bg,[slot*256,A.slot*256]);if(mm.ib){pass.p.setIndexBuffer(mm.ib,'uint32');pass.p.drawIndexed(mm.icount)}else{pass.p.draw(mm.n)}};return rt}"
prim__initRt : JSVal -> JSVal -> JSVal -> JSVal -> Int -> PrimIO JSVal

||| Build a material type's pipelines from its generated WGSL and specs:
||| triangle opaque/blended always, line opaque/blended when the material
||| declares `vs_line`. The bind group layout is decoded from the kinded
||| spec: 0 a uniform buffer, 1 a texture, 2 a sampler.
%foreign "javascript:lambda:(rt,src,bindSpec,meshSpec,meshStride,lineSpec,lineStride,hasLine)=>{const dev=rt.dev;const mod=dev.createShaderModule({code:src});const fmt=navigator.gpu.getPreferredCanvasFormat();const entries=bindSpec.split(';').map(e=>{const a=e.split(',').map(Number);if(a[0]===0)return {binding:a[1],visibility:a[2],buffer:{type:'uniform',hasDynamicOffset:!!a[3],minBindingSize:a[4]}};if(a[0]===1)return {binding:a[1],visibility:2,texture:{}};return {binding:a[1],visibility:2,sampler:{}}});const bgl=dev.createBindGroupLayout({entries:entries});const layout=dev.createPipelineLayout({bindGroupLayouts:[bgl]});const attrsOf=(s)=>s.split(';').map(e=>{const a=e.split(',').map(Number);return {shaderLocation:a[0],offset:a[1],format:a[2]>1?'float32x'+a[2]:'float32'}});const blendState={color:{srcFactor:'src-alpha',dstFactor:'one-minus-src-alpha'},alpha:{srcFactor:'one',dstFactor:'one-minus-src-alpha'}};const mk=(entry,spec,stride,line,blend)=>dev.createRenderPipeline({layout:layout,vertex:{module:mod,entryPoint:entry,buffers:[{arrayStride:stride,attributes:attrsOf(spec)}]},fragment:{module:mod,entryPoint:'fs',targets:[{format:fmt,blend:blend?blendState:undefined}]},primitive:{topology:line?'line-list':'triangle-list',cullMode:line?'none':'back'},depthStencil:{format:'depth24plus',depthWriteEnabled:!blend,depthCompare:'less'}});const matSize=Number(bindSpec.split(';')[2].split(',')[4]);const texCount=bindSpec.split(';').filter(e=>e[0]==='1').length;return rt.mats.push({pl:mk('vs',meshSpec,meshStride,false,false),plb:mk('vs',meshSpec,meshStride,false,true),lpl:hasLine?mk('vs_line',lineSpec,lineStride,true,false):null,lplb:hasLine?mk('vs_line',lineSpec,lineStride,true,true):null,matSize:matSize,texCount:texCount,bgs:{}})-1}"
prim__register : JSVal -> String -> String -> String -> Int -> String -> Int -> Int -> PrimIO Int

||| A material asset: its slot, its textures, its (cached) bind group.
%foreign "javascript:lambda:(rt,mat,slot,t0,t1,t2,t3)=>{const M=rt.mats[mat];return rt.assets.push({mat:mat,slot:slot,t0:t0,t1:t1,t2:t2,t3:t3,bg:rt.bgFor(M,t0,t1,t2,t3),gen:rt.texGen})-1}"
prim__addAsset : JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,ai,t0,t1,t2,t3)=>{const A=rt.assets[ai];const M=rt.mats[A.mat];A.t0=t0;A.t1=t1;A.t2=t2;A.t3=t3;A.bg=rt.bgFor(M,t0,t1,t2,t3);A.gen=rt.texGen;return 0}"
prim__updateAsset : JSVal -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

||| Upload one asset's 256-byte slot from the material scratch: once per
||| add or update, never per draw.
%foreign "javascript:lambda:(rt,a,slot)=>{rt.dev.queue.writeBuffer(rt.mbuf,slot*256,a,slot*64,64);return 0}"
prim__uploadMatSlot : JSVal -> AnyPtr -> Int -> PrimIO Int

||| The engine's gizmo line pipeline and its one bind group (globals + the
||| object buffer, whose lane carries the colour).
%foreign "javascript:lambda:(rt,src,bindSpec,lineSpec,lineStride)=>{const dev=rt.dev;const mod=dev.createShaderModule({code:src});const fmt=navigator.gpu.getPreferredCanvasFormat();const entries=bindSpec.split(';').map(e=>{const a=e.split(',').map(Number);return {binding:a[1],visibility:a[2],buffer:{type:'uniform',hasDynamicOffset:!!a[3],minBindingSize:a[4]}}});const bgl=dev.createBindGroupLayout({entries:entries});const layout=dev.createPipelineLayout({bindGroupLayouts:[bgl]});const attrs=lineSpec.split(';').map(e=>{const a=e.split(',').map(Number);return {shaderLocation:a[0],offset:a[1],format:a[2]>1?'float32x'+a[2]:'float32'}});const pl=dev.createRenderPipeline({layout:layout,vertex:{module:mod,entryPoint:'vs',buffers:[{arrayStride:lineStride,attributes:attrs}]},fragment:{module:mod,entryPoint:'fs',targets:[{format:fmt,blend:{color:{srcFactor:'src-alpha',dstFactor:'one-minus-src-alpha'},alpha:{srcFactor:'one',dstFactor:'one-minus-src-alpha'}}}]},primitive:{topology:'line-list'},depthStencil:{format:'depth24plus',depthWriteEnabled:false,depthCompare:'less'}});const bg=dev.createBindGroup({layout:pl.getBindGroupLayout(0),entries:[{binding:0,resource:{buffer:rt.gbuf}},{binding:1,resource:{buffer:rt.obuf,size:rt.objSize}}]});rt.line={pl:pl,bg:bg};return 0}"
prim__lineInit : JSVal -> String -> String -> String -> Int -> PrimIO Int

||| Reserve a texture id *now*; decode fills the table entry and bumps
||| `texGen` whenever it lands. Reads as 1x1 white until then, or forever
||| if the bytes never decode. usage 22 = TEXTURE_BINDING(4) | COPY_DST(2)
||| | RENDER_ATTACHMENT(16), the last two required by
||| copyExternalImageToTexture -- texture COPY_DST is 2 where buffer
||| COPY_DST is 8, a confusion that surfaces as an invalid bind group far
||| from the mistake.
%foreign "javascript:lambda:(rt,url)=>{const id=rt.texs.push(null)-1;const img=new Image();img.onload=async()=>{try{const bmp=await createImageBitmap(img);const t=rt.dev.createTexture({size:[bmp.width,bmp.height],format:'rgba8unorm-srgb',usage:22});rt.dev.queue.copyExternalImageToTexture({source:bmp},{texture:t},[bmp.width,bmp.height]);rt.texs[id]=t.createView();rt.texGen++}catch(e){console.warn('offler: texture decode failed',e)}};img.onerror=()=>{console.warn('offler: texture load failed: '+url.slice(0,64))};img.src=url;return id}"
prim__loadTexture : JSVal -> String -> PrimIO Int

%foreign "javascript:lambda:(dev,c)=>dev.createTexture({size:[c.width,c.height],format:'depth24plus',usage:16})"
prim__depthTexture : JSVal -> JSVal -> PrimIO JSVal

||| usage 72 = UNIFORM (64) | COPY_DST (8)
%foreign "javascript:lambda:(dev,n)=>dev.createBuffer({size:n,usage:72})"
prim__uniformBuffer : JSVal -> Int -> PrimIO JSVal

||| usage 40 = VERTEX (32) | COPY_DST (8)
%foreign "javascript:lambda:(rt,a,n,topo)=>{const dev=rt.dev;const b=dev.createBuffer({size:a.byteLength,usage:40});dev.queue.writeBuffer(b,0,a);return rt.meshes.push({b:b,n:n,topo:topo,ib:null,icount:0})-1}"
prim__createMesh : JSVal -> AnyPtr -> Int -> Int -> PrimIO Int

||| usage 24 = INDEX (16) | COPY_DST (8)
%foreign "javascript:lambda:(rt,a,n,idx,icount)=>{const dev=rt.dev;const b=dev.createBuffer({size:a.byteLength,usage:40});dev.queue.writeBuffer(b,0,a);const ib=dev.createBuffer({size:idx.byteLength,usage:24});dev.queue.writeBuffer(ib,0,idx);return rt.meshes.push({b:b,n:n,topo:0,ib:ib,icount:icount})-1}"
prim__createMeshIndexed : JSVal -> AnyPtr -> Int -> AnyPtr -> Int -> PrimIO Int

||| The gizmo overlay is the one buffer that gets replaced; the old one
||| must go, and never mid-frame.
%foreign "javascript:lambda:(rt,a,n)=>{if(rt.lineBuf)rt.lineBuf.destroy();const b=rt.dev.createBuffer({size:a.byteLength,usage:40});rt.dev.queue.writeBuffer(b,0,a);rt.lineBuf=b;rt.lineCount=n;return 0}"
prim__setLines : JSVal -> AnyPtr -> Int -> PrimIO Int

%foreign "javascript:lambda:(dev,b,off,a)=>{dev.queue.writeBuffer(b,off,a);return 0}"
prim__write : JSVal -> JSVal -> Int -> AnyPtr -> PrimIO Int

||| Upload only the prefix of `a` that this frame actually filled. `count`
||| is in Float32Array elements.
%foreign "javascript:lambda:(dev,b,a,count)=>{dev.queue.writeBuffer(b,0,a,0,count);return 0}"
prim__writePrefix : JSVal -> JSVal -> AnyPtr -> Int -> PrimIO Int

%foreign "javascript:lambda:(dev,ctx,depth,r,g,b)=>{const e=dev.createCommandEncoder();const p=e.beginRenderPass({colorAttachments:[{view:ctx.getCurrentTexture().createView(),clearValue:{r:r,g:g,b:b,a:1},loadOp:'clear',storeOp:'store'}],depthStencilAttachment:{view:depth.createView(),depthClearValue:1,depthLoadOp:'clear',depthStoreOp:'store'}});return {e:e,p:p}}"
prim__beginPass : JSVal -> JSVal -> JSVal -> Double -> Double -> Double -> PrimIO JSVal

||| One draw: opaque and masked record immediately; blended queue for the
||| sorted flush. The two dynamic offsets pick the draw's object slot and
||| the asset's material slot.
%foreign "javascript:lambda:(rt,pass,asset,mesh,slot,blend,depth)=>{if(blend){rt.pend.push({a:asset,m:mesh,s:slot,d:depth});return 0}rt.exec(pass,asset,mesh,slot,false);return 0}"
prim__draw : JSVal -> JSVal -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

||| The batched form: one foreign call records `count` consecutive object
||| slots against one asset.
%foreign "javascript:lambda:(rt,pass,asset,mesh,first,count,blend,depth)=>{if(blend){for(let i=0;i<count;i++)rt.pend.push({a:asset,m:mesh,s:first+i,d:depth});return 0}const A=rt.assets[asset];const M=rt.mats[A.mat];const mm=rt.meshes[mesh];if(A.gen!==rt.texGen){A.bg=rt.bgFor(M,A.t0,A.t1,A.t2,A.t3);A.gen=rt.texGen}const pipe=mm.topo===1?M.lpl:M.pl;if(!pipe)return 0;pass.p.setPipeline(pipe);pass.p.setVertexBuffer(0,mm.b);if(mm.ib)pass.p.setIndexBuffer(mm.ib,'uint32');for(let i=0;i<count;i++){pass.p.setBindGroup(0,A.bg,[(first+i)*256,A.slot*256]);if(mm.ib)pass.p.drawIndexed(mm.icount);else pass.p.draw(mm.n)}return 0}"
prim__drawSlices : JSVal -> JSVal -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

%foreign "javascript:lambda:(rt,pass,off)=>{if(!rt.lineBuf||rt.lineCount<=0)return 0;pass.p.setPipeline(rt.line.pl);pass.p.setVertexBuffer(0,rt.lineBuf);pass.p.setBindGroup(0,rt.line.bg,[off]);pass.p.draw(rt.lineCount);return 0}"
prim__drawLines : JSVal -> JSVal -> Int -> PrimIO Int

||| The transparent phase: back to front, so blends composite correctly.
%foreign "javascript:lambda:(rt,pass)=>{rt.pend.sort((x,y)=>y.d-x.d);for(const d of rt.pend)rt.exec(pass,d.a,d.m,d.s,true);rt.pend.length=0;return 0}"
prim__flush : JSVal -> JSVal -> PrimIO Int

%foreign "javascript:lambda:(dev,pass)=>{pass.p.end();dev.queue.submit([pass.e.finish()]);return 0}"
prim__submit : JSVal -> JSVal -> PrimIO Int

public export
record Gpu where
  constructor MkGpu
  device, ctx, canvas, rt : JSVal
  globalBuf, objBuf, matBuf : JSVal
  globalScratch : GlobalScratch
  objScratch, matScratch : ObjScratch
  ||| Rebuilt whenever the canvas changes size, so it always matches.
  depth : IORef JSVal
  ||| Eye and forward at `beginFrame`, for sorting the transparent phase.
  eyeFwd : IORef (V3, V3)
  ||| Material assets minted so far: the next free slot of the material
  ||| buffer.
  assetCount : IORef Int
  lineCount : IORef Int

||| Acquire a device, the buffers and the gizmo pipeline. Materials arrive
||| later, through `registerMaterial`/`addMaterial`. Calls back with
||| `Nothing` when no adapter is available, so the page can say so rather
||| than hanging.
export
initGpu : (canvasId : String) -> (Maybe Gpu -> IO ()) -> IO ()
initGpu canvasId k =
  ignore (primIO (prim__requestDevice (\dev => toPrim (withDevice dev >> pure 0))))
  where
    withDevice : JSVal -> IO ()
    withDevice dev = do
      ok <- primIO (prim__isSome dev)
      if ok == 0 then k Nothing else do
        canvas <- byId canvasId
        ignore (syncSize canvas)
        ctx <- primIO (prim__configure dev canvas)
        depthTex <- primIO (prim__depthTexture dev canvas)
        depth <- newIORef depthTex
        gbuf <- primIO (prim__uniformBuffer dev globalSize)
        obuf <- primIO (prim__uniformBuffer dev (objStride * maxObjects))
        mbuf <- primIO (prim__uniformBuffer dev (objStride * maxObjects))
        rt <- primIO (prim__initRt dev gbuf obuf mbuf objSize)
        ignore (primIO (prim__lineInit rt (wgslLinePrologue ++ lineWgslSrc)
                                       lineBindSpec lineVertexSpec lineStride))
        gs <- newGlobalScratch
        os <- newObjScratch
        ms <- newObjScratch
        ef <- newIORef (zero3, MkV3 0.0 0.0 (-1.0))
        ac <- newIORef 0
        lc <- newIORef 0
        k (Just (MkGpu dev ctx canvas rt gbuf obuf mbuf gs os ms depth ef ac lc))

||| The pass and the slot counter, held in the linear token: a draw cannot
||| see a pass that has ended, and cannot read a slot index left over from
||| the previous frame.
export
data GpuFrame : Type where
  MkGpuFrame : (pass : JSVal) -> (nextSlot : Int) -> GpuFrame

||| View-space depth of a model's translation, for the transparent sort.
depthOf : Gpu -> Mat4 -> IO Double
depthOf r model = do
  (eye, fwd) <- readIORef r.eyeFwd
  pure (dot3 fwd (sub3 (MkV3 model.m12 model.m13 model.m14) eye))

||| Write a material value into an asset slot and its bindings backend-side.
fillAsset : Material m => Gpu -> (slotIdx : Int) -> m -> IO ()
fillAsset r i v =
  case slot r.matScratch i of
    Nothing => pure ()
    Just s => do
      writeMat (matWriter r.matScratch s) v
      ignore (primIO (prim__uploadMatSlot r.rt (raw r.matScratch) (slotIndex s)))

export
Renderer Gpu GpuFrame where
  rendererName _ = "WebGPU"

  createMesh {t} r vs =
    meshHandle <$> primIO (prim__createMesh r.rt (vertsRaw vs) (vertsCount vs)
                                            (topoCode t))

  createMeshIndexed r vs ix =
    meshHandle <$> primIO (prim__createMeshIndexed r.rt (vertsRaw vs) (vertsCount vs)
                                                   (indicesRaw ix) (indicesCount ix))

  loadTexture r src =
    let url = case src of
                FromPath p => p
                FromBase64 mime b64 => "data:" ++ mime ++ ";base64," ++ b64
     in textureHandle <$> primIO (prim__loadTexture r.rt url)

  registerMaterial r {m} = do
    i <- primIO (prim__register r.rt (materialWgsl {m}) (materialSpec {m})
                                meshVertexSpec meshStride lineVertexSpec lineStride
                                (if matLineEntry {m} then 1 else 0))
    pure (materialId i)

  addMaterial r mid v = do
    i <- readIORef r.assetCount
    writeIORef r.assetCount (i + 1)
    fillAsset r i v
    let (t0, t1, t2, t3) = texIds v
    a <- primIO (prim__addAsset r.rt (materialIdIndex mid) i t0 t1 t2 t3)
    pure (handleFor a (alphaMode v))

  updateMaterial r h v = do
    let a = handleAsset h
    fillAsset r a v
    let (t0, t1, t2, t3) = texIds v
    ignore (primIO (prim__updateAsset r.rt a t0 t1 t2 t3))
    pure (handleFor a (alphaMode v))

  setLines r vs = do
    ignore (primIO (prim__setLines r.rt (vertsRaw vs) (vertsCount vs)))
    writeIORef r.lineCount (vertsCount vs)

  aspect r = primIO (prim__aspect r.canvas)

  -- Also where resizes are absorbed: getCurrentTexture() follows the
  -- canvas automatically, but the depth attachment does not, so it is
  -- rebuilt whenever the drawing buffer moved.
  beginFrame r cam lights t = do
    p <- liftIO $ do
      changed <- syncSize r.canvas
      when changed $ do
        tex <- primIO (prim__depthTexture r.device r.canvas)
        writeIORef r.depth tex
      ratio <- primIO (prim__aspect r.canvas)
      pokeGlobals r.globalScratch cam ratio lights t True
      ignore (primIO (prim__write r.device r.globalBuf 0 (raw r.globalScratch)))
      writeIORef r.eyeFwd
        (eyeOf cam, qRotate cam.transform.rotation (MkV3 0.0 0.0 (-1.0)))
      depthTex <- readIORef r.depth
      let cc = cam.clearColor
      primIO (prim__beginPass r.device r.ctx depthTex cc.red cc.green cc.blue)
    pure1 (Just (MkGpuFrame p 0))

  draw r (MkGpuFrame p i) mesh h model =
    case slot r.objScratch i of
      Nothing => pure1 (MkGpuFrame p i)
      Just s => do
        liftIO $ do
          pokeObject r.objScratch s model (handleCode h) (handleCutoff h) 0.0 0.0
          d <- if handleBlend h then depthOf r model else pure 0.0
          ignore (primIO (prim__draw r.rt p (handleAsset h) (meshIndex mesh)
                                     (slotIndex s) (if handleBlend h then 1 else 0) d))
        pure1 (MkGpuFrame p (i + 1))

  drawMany r (MkGpuFrame p i) mesh h models = do
    i' <- liftIO $ case models of
      [] => pure i
      (mdl0 :: _) => do
        filled <- fillModels r.objScratch i (handleCode h) (handleCutoff h) models
        let count = filled - i
        when (count > 0) $ do
          d <- if handleBlend h then depthOf r mdl0 else pure 0.0
          ignore (primIO (prim__drawSlices r.rt p (handleAsset h) (meshIndex mesh)
                                           i count (if handleBlend h then 1 else 0) d))
        pure filled
    pure1 (MkGpuFrame p i')

  drawLines r (MkGpuFrame p i) colour =
    case slot r.objScratch i of
      Nothing => pure1 (MkGpuFrame p i)
      Just s => do
        liftIO $ do
          n <- readIORef r.lineCount
          when (n > 0) $ do
            pokeObject r.objScratch s identity
                       colour.red colour.green colour.blue colour.alpha
            ignore (primIO (prim__drawLines r.rt p (slotOffset s)))
        pure1 (MkGpuFrame p (i + 1))

  endFrame r (MkGpuFrame p i) = liftIO $ do
    -- The sorted transparent phase records last, over the finished opaque
    -- scene.
    ignore (primIO (prim__flush r.rt p))
    -- One upload for every draw's engine block. Material data went up when
    -- the assets were made. Queue writes are ordered before the submit
    -- that follows.
    ignore (primIO (prim__writePrefix r.device r.objBuf (raw r.objScratch) (i * objFloats)))
    ignore (primIO (prim__submit r.device p))
