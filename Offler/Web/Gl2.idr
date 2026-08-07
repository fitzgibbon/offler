||| WebGL2 implementation of `Renderer`.
|||
||| Modelled on the WebGPU backends rather than classic GL: uniforms live in
||| std140 buffer blocks -- which lay out identically to WGSL's uniform
||| address space for offler's field types -- so the same scratch buffers
||| and the same 256-byte slots serve here byte for byte, selected per draw
||| with `bindBufferRange`, GL's spelling of a dynamic offset. Material
||| assets own a slot of the material buffer, uploaded once when made;
||| programs are per material type, triangle and (when declared) line
||| variants. Draws are recorded and replayed at `endFrame` after one
||| prefix upload of the object buffer -- per-draw `bufferSubData` was
||| measured at a quarter of the frame rate at ten thousand bodies --
||| opaque in call order, then gizmos, then `Blend` depth-sorted.
module Offler.Web.Gl2

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
import Offler.Web.Js

-- `Control.Linear.LIO`, re-exported by Offler.Gfx.Renderer, defines
-- `fromInteger` for its `Usage` type, which makes bare integer literals
-- ambiguous.
%hide Control.Linear.LIO.fromInteger

%default covering

%foreign "javascript:lambda:(c)=>c.getContext('webgl2',{antialias:true})"
prim__context : JSVal -> PrimIO JSVal

||| The mutable JS runtime: buffers, tables, the frame's recorded draws, and
||| the replay executors with their binding caches -- defined once here
||| because each `%foreign` lambda is otherwise its own world. Buffer sizes
||| and strides arrive as arguments from the layout.
%foreign "javascript:lambda:(gl,meshSpec,meshStride,lineSpec,lineStride,instSpec,instStride,gSize,objSize,stride,maxN)=>{const rt={gl:gl,maxN:maxN,stride:stride,meshes:[],freeMeshes:[],insts:[],texs:[],mats:[],assets:[],all:[],obufs:[],mbufs:[],lineBuf:null,lineCount:0,lineProg:null,matArr:null,curProg:null,curMesh:null,curTex:null};gl.enable(gl.DEPTH_TEST);gl.enable(gl.CULL_FACE);const mkbuf=(n)=>{const b=gl.createBuffer();gl.bindBuffer(gl.UNIFORM_BUFFER,b);gl.bufferData(gl.UNIFORM_BUFFER,n,gl.DYNAMIC_DRAW);return b};rt.gbuf=mkbuf(gSize);rt.pageOf=(arr,p)=>{while(arr.length<=p)arr.push(mkbuf(stride*maxN));return arr[p]};gl.bindBufferBase(gl.UNIFORM_BUFFER,0,rt.gbuf);rt.white=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,rt.white);gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA8,1,1,0,gl.RGBA,gl.UNSIGNED_BYTE,new Uint8Array([255,255,255,255]));const meshAttrs=meshSpec.split(';').map(e=>e.split(',').map(Number));const lineAttrs=lineSpec.split(';').map(e=>e.split(',').map(Number));rt.bindMesh=(mm)=>{gl.bindBuffer(gl.ARRAY_BUFFER,mm.b);const attrs=mm.topo===1?lineAttrs:meshAttrs;const stridev=mm.topo===1?lineStride:meshStride;for(const a of attrs){gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,stridev,a[1])}if(mm.topo===1)gl.disableVertexAttribArray(2);if(mm.ib)gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,mm.ib)};rt.execDraw=(d)=>{const A=rt.assets[d.a];const M=rt.mats[A.mat];const mm=rt.meshes[d.m];if(!mm||!mm.b||mm.gen!==d.g)return;const prog=mm.topo===1?M.lineProg:M.prog;if(!prog)return;if(rt.curProg!==prog){gl.useProgram(prog);rt.curProg=prog}gl.bindBufferRange(gl.UNIFORM_BUFFER,1,rt.pageOf(rt.obufs,Math.floor(d.s/maxN)),(d.s%maxN)*stride,objSize);gl.bindBufferRange(gl.UNIFORM_BUFFER,2,rt.pageOf(rt.mbufs,Math.floor(A.slot/maxN)),(A.slot%maxN)*stride,M.matSize);const tk=A.mat+':'+A.t0+','+A.t1+','+A.t2+','+A.t3;if(rt.curTex!==tk){const ts=[A.t0,A.t1,A.t2,A.t3];for(let i=0;i<M.texCount;i++){gl.activeTexture(gl.TEXTURE0+i);gl.bindTexture(gl.TEXTURE_2D,ts[i]>=0&&rt.texs[ts[i]]?rt.texs[ts[i]]:rt.white)}rt.curTex=tk}if(rt.curMesh!==mm){rt.bindMesh(mm);rt.curMesh=mm}if(mm.ib)gl.drawElements(mm.topo===1?gl.LINES:gl.TRIANGLES,mm.icount,gl.UNSIGNED_INT,0);else gl.drawArrays(mm.topo===1?gl.LINES:gl.TRIANGLES,0,mm.n)};rt.execLine=(d)=>{if(!rt.lineBuf||rt.lineCount<=0)return;gl.useProgram(rt.lineProg);rt.curProg=null;gl.bindBufferRange(gl.UNIFORM_BUFFER,1,rt.pageOf(rt.obufs,Math.floor(d.s/maxN)),(d.s%maxN)*stride,objSize);gl.bindBuffer(gl.ARRAY_BUFFER,rt.lineBuf);for(const a of lineAttrs){gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,lineStride,a[1])}gl.disableVertexAttribArray(2);rt.curMesh=null;gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);gl.depthMask(false);gl.drawArrays(gl.LINES,0,rt.lineCount);gl.depthMask(true);gl.disable(gl.BLEND)};const instAttrs=instSpec.split(';').map(e=>e.split(',').map(Number));rt.execInst=(d)=>{const A=rt.assets[d.a];const M=rt.mats[A.mat];const mm=rt.meshes[d.m];const I=rt.insts[d.i];if(!mm||!mm.b||mm.gen!==d.g||!I||!I.b||I.n<=0||!M.instProg)return;gl.useProgram(M.instProg);rt.curProg=null;gl.bindBufferRange(gl.UNIFORM_BUFFER,1,rt.pageOf(rt.obufs,Math.floor(d.s/maxN)),(d.s%maxN)*stride,objSize);gl.bindBufferRange(gl.UNIFORM_BUFFER,2,rt.pageOf(rt.mbufs,Math.floor(A.slot/maxN)),(A.slot%maxN)*stride,M.matSize);const ts=[A.t0,A.t1,A.t2,A.t3];for(let i=0;i<M.texCount;i++){gl.activeTexture(gl.TEXTURE0+i);gl.bindTexture(gl.TEXTURE_2D,ts[i]>=0&&rt.texs[ts[i]]?rt.texs[ts[i]]:rt.white)}rt.curTex=null;gl.bindBuffer(gl.ARRAY_BUFFER,mm.b);for(const a of meshAttrs){gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,meshStride,a[1]);gl.vertexAttribDivisor(a[0],0)}gl.bindBuffer(gl.ARRAY_BUFFER,I.b);for(const a of instAttrs){gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,instStride,a[1]);gl.vertexAttribDivisor(a[0],1)}if(mm.ib){gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,mm.ib);gl.drawElementsInstanced(gl.TRIANGLES,mm.icount,gl.UNSIGNED_INT,0,I.n)}else{gl.drawArraysInstanced(gl.TRIANGLES,0,mm.n,I.n)}for(const a of instAttrs){gl.vertexAttribDivisor(a[0],0);gl.disableVertexAttribArray(a[0])}rt.curMesh=null};return rt}"
prim__initRt : JSVal -> String -> Int -> String -> Int -> String -> Int -> Int -> Int -> Int -> Int -> PrimIO JSVal

||| The material staging array, attached once it exists so slot uploads can
||| read it.
%foreign "javascript:lambda:(rt,m)=>{rt.matArr=m;return 0}"
prim__attach : JSVal -> AnyPtr -> PrimIO Int

||| Compile and link a material's program(s), wire the uniform blocks to
||| the fixed binding points by name (a block a shader never reads is
||| optimised out; skip it), and point samplers at consecutive texture
||| units. The line program exists only when the material declares its
||| line stage. Throws with the driver's log if compilation fails, which
||| the page's error box then shows.
%foreign "javascript:lambda:(rt,vs,fs,lvs,hasLine,ivs,hasInst,texNames,matSize)=>{const gl=rt.gl;const mk=(t,s)=>{const o=gl.createShader(t);gl.shaderSource(o,s);gl.compileShader(o);if(!gl.getShaderParameter(o,gl.COMPILE_STATUS))throw new Error(gl.getShaderInfoLog(o));return o};const link=(vsrc)=>{const p=gl.createProgram();gl.attachShader(p,mk(gl.VERTEX_SHADER,vsrc));gl.attachShader(p,mk(gl.FRAGMENT_SHADER,fs));gl.linkProgram(p);if(!gl.getProgramParameter(p,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(p));gl.useProgram(p);['Globals','Obj','Mat'].forEach((n,i)=>{const bi=gl.getUniformBlockIndex(p,n);if(bi!==0xFFFFFFFF)gl.uniformBlockBinding(p,bi,i)});const names=texNames?texNames.split(';'):[];names.forEach((n,i)=>{const l=gl.getUniformLocation(p,'t_'+n);if(l)gl.uniform1i(l,i)});return p};const names=texNames?texNames.split(';'):[];return rt.mats.push({prog:link(vs),lineProg:hasLine?link(lvs):null,instProg:hasInst?link(ivs):null,texCount:names.length,matSize:matSize})-1}"
prim__register : JSVal -> String -> String -> String -> Int -> String -> Int -> String -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,mat,slot,t0,t1,t2,t3)=>rt.assets.push({mat:mat,slot:slot,t0:t0,t1:t1,t2:t2,t3:t3})-1"
prim__addAsset : JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,ai,t0,t1,t2,t3)=>{const A=rt.assets[ai];A.t0=t0;A.t1=t1;A.t2=t2;A.t3=t3;rt.curTex=null;return 0}"
prim__updateAsset : JSVal -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

||| Upload one asset's 256-byte slot from the staging scratch (`local` the
||| staging slot, `global` the asset's slot across the paged buffers): once
||| per add or update, never per draw.
%foreign "javascript:lambda:(rt,local,global)=>{const gl=rt.gl;gl.bindBuffer(gl.UNIFORM_BUFFER,rt.pageOf(rt.mbufs,Math.floor(global/rt.maxN)));gl.bufferSubData(gl.UNIFORM_BUFFER,(global%rt.maxN)*rt.stride,rt.matArr,local*64,64);return 0}"
prim__uploadMatSlot : JSVal -> Int -> Int -> PrimIO Int

||| The engine's gizmo line program, against the same blocks.
%foreign "javascript:lambda:(rt,vs,fs)=>{const gl=rt.gl;const mk=(t,s)=>{const o=gl.createShader(t);gl.shaderSource(o,s);gl.compileShader(o);if(!gl.getShaderParameter(o,gl.COMPILE_STATUS))throw new Error(gl.getShaderInfoLog(o));return o};const p=gl.createProgram();gl.attachShader(p,mk(gl.VERTEX_SHADER,vs));gl.attachShader(p,mk(gl.FRAGMENT_SHADER,fs));gl.linkProgram(p);if(!gl.getProgramParameter(p,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(p));gl.useProgram(p);['Globals','Obj'].forEach((n,i)=>{const bi=gl.getUniformBlockIndex(p,n);if(bi!==0xFFFFFFFF)gl.uniformBlockBinding(p,bi,i)});rt.lineProg=p;return 0}"
prim__lineInit : JSVal -> String -> String -> PrimIO Int

||| Reserve a texture id *now*; the decode fills the table entry whenever
||| it lands (bindings are looked up per draw here, so late textures are
||| picked up with no extra machinery). sRGB, linear filtering, repeat.
%foreign "javascript:lambda:(rt,url)=>{const gl=rt.gl;const id=rt.texs.push(null)-1;const img=new Image();img.onload=()=>{try{const t=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,t);gl.texImage2D(gl.TEXTURE_2D,0,gl.SRGB8_ALPHA8,gl.RGBA,gl.UNSIGNED_BYTE,img);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.LINEAR);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MAG_FILTER,gl.LINEAR);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_S,gl.REPEAT);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_T,gl.REPEAT);rt.texs[id]=t;rt.curTex=null}catch(e){console.warn('offler: texture decode failed',e)}};img.onerror=()=>{console.warn('offler: texture load failed: '+url.slice(0,64))};img.src=url;return id}"
prim__loadTexture : JSVal -> String -> PrimIO Int

||| Indices come off the free list before the table grows; a recycled entry
||| keeps its bumped generation.
%foreign "javascript:lambda:(rt,a,n,topo)=>{const gl=rt.gl;const b=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,b);gl.bufferData(gl.ARRAY_BUFFER,a,gl.STATIC_DRAW);const i=rt.freeMeshes.length?rt.freeMeshes.pop():(rt.meshes.push(null)-1);const g=rt.meshes[i]?rt.meshes[i].gen:0;rt.meshes[i]={b:b,n:n,topo:topo,ib:null,icount:0,gen:g};return i}"
prim__createMesh : JSVal -> AnyPtr -> Int -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,a,n,idx,icount)=>{const gl=rt.gl;const b=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,b);gl.bufferData(gl.ARRAY_BUFFER,a,gl.STATIC_DRAW);const ib=gl.createBuffer();gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER,ib);gl.bufferData(gl.ELEMENT_ARRAY_BUFFER,idx,gl.STATIC_DRAW);const i=rt.freeMeshes.length?rt.freeMeshes.pop():(rt.meshes.push(null)-1);const g=rt.meshes[i]?rt.meshes[i].gen:0;rt.meshes[i]={b:b,n:n,topo:0,ib:ib,icount:icount,gen:g};return i}"
prim__createMeshIndexed : JSVal -> AnyPtr -> Int -> AnyPtr -> Int -> PrimIO Int

||| The generation an index was (re)minted at, read back once at create.
%foreign "javascript:lambda:(rt,i)=>{const mm=rt.meshes[i];return mm?mm.gen:0}"
prim__meshGen : JSVal -> Int -> PrimIO Int

||| Delete the buffers, bump the generation and recycle the index;
||| `execDraw` skips draws whose handle generation no longer matches, so
||| stale handles are silent, not fatal. A stale free is a no-op likewise.
%foreign "javascript:lambda:(rt,mi,gen)=>{const gl=rt.gl;const mm=rt.meshes[mi];if(!mm||!mm.b||mm.gen!==gen)return 0;gl.deleteBuffer(mm.b);if(mm.ib)gl.deleteBuffer(mm.ib);mm.b=null;mm.ib=null;mm.gen++;rt.freeMeshes.push(mi);return 0}"
prim__freeMesh : JSVal -> Int -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt,a,n)=>{const gl=rt.gl;if(!rt.lineBuf)rt.lineBuf=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,rt.lineBuf);gl.bufferData(gl.ARRAY_BUFFER,a,gl.STATIC_DRAW);rt.lineCount=n;return 0}"
prim__setLines : JSVal -> AnyPtr -> Int -> PrimIO Int

%foreign "javascript:lambda:(gl)=>gl.drawingBufferWidth/gl.drawingBufferHeight"
prim__aspect : JSVal -> PrimIO Double

%foreign "javascript:lambda:(gl)=>{gl.viewport(0,0,gl.drawingBufferWidth,gl.drawingBufferHeight);return 0}"
prim__viewport : JSVal -> PrimIO Int

||| Upload the frame globals and clear. GL executes immediately, so the
||| globals must land before the first draw rather than at submit.
%foreign "javascript:lambda:(rt,garr,count,r,g,b)=>{const gl=rt.gl;gl.bindBuffer(gl.UNIFORM_BUFFER,rt.gbuf);gl.bufferSubData(gl.UNIFORM_BUFFER,0,garr,0,count);gl.clearColor(r,g,b,1);gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);return 0}"
prim__begin : JSVal -> AnyPtr -> Int -> Double -> Double -> Double -> PrimIO Int

||| One draw, recorded: kind 0 opaque/masked, 2 blended.
%foreign "javascript:lambda:(rt,asset,mesh,gen,slot,blend,depth)=>{rt.all.push({k:blend?2:0,a:asset,m:mesh,g:gen,s:slot,d:depth});return 0}"
prim__draw : JSVal -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

||| The batched form: one foreign call records `count` consecutive slots.
%foreign "javascript:lambda:(rt,asset,mesh,gen,first,count,blend,depth)=>{for(let i=0;i<count;i++)rt.all.push({k:blend?2:0,a:asset,m:mesh,g:gen,s:first+i,d:depth});return 0}"
prim__drawSlices : JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

||| A gizmo overlay draw, recorded (kind 1).
%foreign "javascript:lambda:(rt,slot)=>{rt.all.push({k:1,s:slot});return 0}"
prim__drawLines : JSVal -> Int -> PrimIO Int

%foreign "javascript:lambda:(rt)=>rt.insts.push({b:null,n:0})-1"
prim__createInstances : JSVal -> PrimIO Int

||| Replace an instance buffer's contents (orphaning `bufferData`, the
||| idiomatic dynamic-stream path). `floats` is the used prefix.
%foreign "javascript:lambda:(rt,ih,a,floats,count)=>{const gl=rt.gl;const I=rt.insts[ih];if(!I.b)I.b=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,I.b);gl.bufferData(gl.ARRAY_BUFFER,a.subarray(0,floats),gl.DYNAMIC_DRAW);I.n=count;rt.curMesh=null;return 0}"
prim__writeInstances : JSVal -> Int -> AnyPtr -> Int -> Int -> PrimIO Int

||| An instanced draw, recorded (kind 3): executed in the opaque phase.
%foreign "javascript:lambda:(rt,asset,mesh,gen,slot,ih)=>{rt.all.push({k:3,a:asset,m:mesh,g:gen,s:slot,i:ih});return 0}"
prim__drawInstanced : JSVal -> Int -> Int -> Int -> Int -> Int -> PrimIO Int

||| Upload one object page's used prefix, before the replay. `count` is in
||| Float32Array elements.
%foreign "javascript:lambda:(rt,page,a,count)=>{const gl=rt.gl;gl.bindBuffer(gl.UNIFORM_BUFFER,rt.pageOf(rt.obufs,page));gl.bufferSubData(gl.UNIFORM_BUFFER,0,a,0,count);return 0}"
prim__uploadObjPage : JSVal -> Int -> AnyPtr -> Int -> PrimIO Int

||| The frame's replay: the opaque phase in call order, the gizmo
||| overlays, and the transparent phase back to front, blended and
||| depth-read-only. The object pages were uploaded just before.
%foreign "javascript:lambda:(rt)=>{const gl=rt.gl;rt.curProg=null;rt.curMesh=null;rt.curTex=null;for(const d of rt.all)if(d.k===0)rt.execDraw(d);for(const d of rt.all)if(d.k===3)rt.execInst(d);for(const d of rt.all)if(d.k===1)rt.execLine(d);const bl=rt.all.filter(d=>d.k===2);if(bl.length){bl.sort((a,b)=>b.d-a.d);gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);gl.depthMask(false);for(const d of bl)rt.execDraw(d);gl.depthMask(true);gl.disable(gl.BLEND)}rt.all.length=0;return 0}"
prim__flush : JSVal -> PrimIO Int

public export
record Gl2 where
  constructor MkGl2
  gl, canvas, rt : JSVal
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
  lineCount : IORef Int

||| Build a WebGL2 renderer over the given canvas. Synchronous, unlike
||| WebGPU; materials arrive later, through `registerMaterial`/`addMaterial`.
export
initGl2 : (canvasId : String) -> IO Gl2
initGl2 canvasId = do
  canvas <- byId canvasId
  ignore (syncSize canvas)
  gl <- primIO (prim__context canvas)
  rt <- primIO (prim__initRt gl meshVertexSpec meshStride lineVertexSpec lineStride
                             instanceSpec instanceStride
                             globalSize objSize objStride maxObjects)
  ignore (primIO (prim__viewport gl))
  ignore (primIO (prim__lineInit rt (glslLineVert lineVertSrc)
                                 (glslLineFrag lineFragSrc)))
  gs <- newGlobalScratch
  os <- newPaged
  ms <- newObjScratch
  ignore (primIO (prim__attach rt (raw ms)))
  ef <- newIORef (zero3, MkV3 0.0 0.0 (-1.0))
  MkGl2 gl canvas rt gs os ms ef <$> newIORef 0 <*> newIORef 0

||| WebGL2 keeps no pass object, so the token is pure capability plus the
||| slot counter: it exists to be threaded, and its constructor never
||| leaves this module.
export
data Gl2Frame : Type where
  MkGl2Frame : (nextSlot : Int) -> Gl2Frame

||| View-space depth of a model's translation, for the transparent sort.
depthOf : Gl2 -> Mat4 -> IO Double
depthOf r model = do
  (eye, fwd) <- readIORef r.eyeFwd
  pure (dot3 fwd (sub3 (MkV3 model.m12 model.m13 model.m14) eye))

||| Write a material value into an asset slot and upload it. The staging
||| slot is `i mod maxObjects`; the upload lands on slot `i` of the paged
||| material buffers.
fillAsset : Material m => Gl2 -> (slotIdx : Int) -> m -> IO ()
fillAsset r i v =
  case slot r.matScratch (i `mod` maxObjects) of
    Nothing => pure ()
    Just s => do
      writeMat (matWriter r.matScratch s) v
      ignore (primIO (prim__uploadMatSlot r.rt (slotIndex s) i))

export
Renderer Gl2 Gl2Frame where
  rendererName _ = "WebGL2"

  createMesh {t} r vs = do
    i <- primIO (prim__createMesh r.rt (vertsRaw vs) (vertsCount vs) (topoCode t))
    g <- primIO (prim__meshGen r.rt i)
    pure (meshHandle i g)

  createMeshIndexed r vs ix = do
    i <- primIO (prim__createMeshIndexed r.rt (vertsRaw vs) (vertsCount vs)
                                         (indicesRaw ix) (indicesCount ix))
    g <- primIO (prim__meshGen r.rt i)
    pure (meshHandle i g)

  freeMesh r mesh =
    ignore (primIO (prim__freeMesh r.rt (meshIndex mesh) (meshGen mesh)))

  loadTexture r src =
    let url = case src of
                FromPath p => p
                FromBase64 mime b64 => "data:" ++ mime ++ ";base64," ++ b64
     in textureHandle <$> primIO (prim__loadTexture r.rt url)

  registerMaterial r {m} = do
    i <- primIO (prim__register r.rt (materialGlslVert {m}) (materialGlslFrag {m})
                                (materialGlslLineVert {m})
                                (if matLineEntry {m} then 1 else 0)
                                (materialGlslInstVert {m})
                                (if matInstEntry {m} then 1 else 0)
                                (materialTexNames {m}) (materialSize {m}))
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

  setGizmos r vs = do
    ignore (primIO (prim__setLines r.rt (vertsRaw vs) (vertsCount vs)))
    writeIORef r.lineCount (vertsCount vs)

  aspect r = primIO (prim__aspect r.gl)

  -- Also where resizes are absorbed: the drawing buffer is the viewport,
  -- so re-syncing it is all a size change needs.
  beginFrame r cam lights t = do
    liftIO $ do
      changed <- syncSize r.canvas
      when changed (ignore (primIO (prim__viewport r.gl)))
      ratio <- primIO (prim__aspect r.gl)
      pokeGlobals r.globalScratch cam ratio lights t False
      writeIORef r.eyeFwd
        (eyeOf cam, qRotate cam.transform.rotation (MkV3 0.0 0.0 (-1.0)))
      let cc = cam.clearColor
      ignore (primIO (prim__begin r.rt (raw r.globalScratch) globalFloats
                                  cc.red cc.green cc.blue))
    pure1 (Just (MkGl2Frame 0))

  draw r (MkGl2Frame i) mesh h model = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          pokeObject arr s model (handleCode h) (handleCutoff h) 0.0 0.0
          d <- if handleBlend h then depthOf r model else pure 0.0
          ignore (primIO (prim__draw r.rt (handleAsset h) (meshIndex mesh)
                                     (meshGen mesh) i
                                     (if handleBlend h then 1 else 0) d))
    pure1 (MkGl2Frame (i + 1))

  -- Chunked at page boundaries, so each recorded run's slots share a page.
  drawMany r (MkGl2Frame i) mesh h models = do
    i' <- liftIO (goChunks i models)
    pure1 (MkGl2Frame i')
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
              ignore (primIO (prim__drawSlices r.rt (handleAsset h)
                                               (meshIndex mesh) (meshGen mesh)
                                               i count
                                               (if handleBlend h then 1 else 0) d))
              goChunks (i + count) rest

  createInstances r = instanceHandle <$> primIO (prim__createInstances r.rt)

  writeInstances r ih sl =
    ignore (primIO (prim__writeInstances r.rt (instanceIndex ih) (instRaw sl)
                                         (instCount sl * instanceFloats)
                                         (instCount sl)))

  drawInstanced r (MkGl2Frame i) mesh h ih model = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          pokeObject arr s model (handleCode h) (handleCutoff h) 0.0 0.0
          ignore (primIO (prim__drawInstanced r.rt (handleAsset h)
                                              (meshIndex mesh) (meshGen mesh)
                                              i (instanceIndex ih)))
    pure1 (MkGl2Frame (i + 1))

  drawGizmos r (MkGl2Frame i) = do
    liftIO $ do
      mp <- pageSlot r.objScratch i
      case mp of
        Nothing => pure ()
        Just (_, arr, s) => do
          n <- readIORef r.lineCount
          when (n > 0) $ do
            -- Identity model, white lane: colours are per vertex.
            pokeObject arr s identity 1.0 1.0 1.0 1.0
            ignore (primIO (prim__drawLines r.rt i))
    pure1 (MkGl2Frame (i + 1))

  -- Upload each touched object page once, then replay the recorded frame:
  -- opaque, gizmos, sorted transparency.
  endFrame r (MkGl2Frame i) = liftIO $ do
    pages <- usedPages r.objScratch i
    traverse_ (\(pg, arr, floats) =>
                 ignore (primIO (prim__uploadObjPage r.rt pg (raw arr) floats)))
              pages
    ignore (primIO (prim__flush r.rt))
