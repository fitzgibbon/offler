||| WebGL2 implementation of `Renderer`.
|||
||| Modelled on the WebGPU backends rather than classic GL: uniforms live in
||| std140 buffer blocks -- which lay out identically to WGSL's uniform
||| address space for offler's field types -- so the same scratch buffers and
||| the same 256-byte slots serve here byte for byte, selected per draw with
||| `bindBufferRange`, GL's spelling of a dynamic offset. Draws are recorded
||| and replayed at `endFrame` after one prefix upload of each slot buffer,
||| the same submit discipline as the other backends. Programs are per
||| material type; `Blend` draws replay depth-sorted, after everything else.
module Offler.Web.Gl2

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

%foreign "javascript:lambda:(c)=>c.getContext('webgl2',{antialias:true})"
prim__context : JSVal -> PrimIO JSVal

||| The mutable JS runtime: buffers, tables, the frame's recorded draws, and
||| the replay executors with their binding caches -- defined once here
||| because each `%foreign` lambda is otherwise its own world. GL executes
||| immediately, so draws are *recorded* and replayed at `endFrame` after a
||| single prefix upload of each slot buffer, exactly as the WebGPU-flavoured
||| backends submit: per-draw `bufferSubData` was measured at a quarter of
||| the frame rate at ten thousand bodies. Buffer sizes and strides arrive
||| as arguments from the layout.
%foreign "javascript:lambda:(gl,meshSpec,meshStride,gSize,objSize,stride,maxN)=>{const rt={gl:gl,meshes:[],texs:[],mats:[],all:[],lineBuf:null,lineCount:0,lineProg:null,objArr:null,matArr:null,curProg:null,curMesh:null,curTex:null};gl.enable(gl.DEPTH_TEST);gl.enable(gl.CULL_FACE);const mkbuf=(n)=>{const b=gl.createBuffer();gl.bindBuffer(gl.UNIFORM_BUFFER,b);gl.bufferData(gl.UNIFORM_BUFFER,n,gl.DYNAMIC_DRAW);return b};rt.gbuf=mkbuf(gSize);rt.obuf=mkbuf(stride*maxN);rt.mbuf=mkbuf(stride*maxN);gl.bindBufferBase(gl.UNIFORM_BUFFER,0,rt.gbuf);rt.white=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,rt.white);gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA8,1,1,0,gl.RGBA,gl.UNSIGNED_BYTE,new Uint8Array([255,255,255,255]));const attrs=meshSpec.split(';').map(e=>e.split(',').map(Number));rt.bindMesh=(mm)=>{gl.bindBuffer(gl.ARRAY_BUFFER,mm.b);for(const a of attrs){gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,meshStride,a[1])}};rt.execDraw=(d)=>{const M=rt.mats[d.mat];const mm=rt.meshes[d.mesh];const off=d.slot*stride;if(rt.curProg!==M.prog){gl.useProgram(M.prog);rt.curProg=M.prog}gl.bindBufferRange(gl.UNIFORM_BUFFER,1,rt.obuf,off,objSize);gl.bindBufferRange(gl.UNIFORM_BUFFER,2,rt.mbuf,off,M.matSize);const tk=d.mat+':'+d.t0+','+d.t1+','+d.t2+','+d.t3;if(rt.curTex!==tk){const ts=[d.t0,d.t1,d.t2,d.t3];for(let i=0;i<M.texCount;i++){gl.activeTexture(gl.TEXTURE0+i);gl.bindTexture(gl.TEXTURE_2D,ts[i]>=0&&rt.texs[ts[i]]?rt.texs[ts[i]]:rt.white)}rt.curTex=tk}if(rt.curMesh!==mm){rt.bindMesh(mm);rt.curMesh=mm}gl.drawArrays(gl.TRIANGLES,0,mm.n)};rt.execLine=(d)=>{if(!rt.lineBuf||rt.lineCount<=0)return;gl.useProgram(rt.lineProg);rt.curProg=null;const off=d.slot*stride;gl.bindBufferRange(gl.UNIFORM_BUFFER,1,rt.obuf,off,objSize);gl.bindBuffer(gl.ARRAY_BUFFER,rt.lineBuf);gl.enableVertexAttribArray(0);gl.vertexAttribPointer(0,3,gl.FLOAT,false,16,0);gl.disableVertexAttribArray(1);gl.disableVertexAttribArray(2);rt.curMesh=null;gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);gl.depthMask(false);gl.drawArrays(gl.LINES,0,rt.lineCount);gl.depthMask(true);gl.disable(gl.BLEND)};return rt}"
prim__initRt : JSVal -> String -> Int -> Int -> Int -> Int -> Int -> PrimIO JSVal

||| The scratch arrays, attached once they exist so `execDraw` can read
||| slots from them at replay time as well as immediately.
%foreign "javascript:lambda:(rt,o,m)=>{rt.objArr=o;rt.matArr=m;return 0}"
prim__attach : JSVal -> AnyPtr -> AnyPtr -> PrimIO Int

||| Compile and link a material's program, wire its uniform blocks to the
||| fixed binding points by name (a block a shader never reads is optimised
||| out; skip it), and point its samplers at consecutive texture units.
||| Throws with the driver's log if compilation fails, which the page's
||| error box then shows.
%foreign "javascript:lambda:(rt,vs,fs,texNames,matSize)=>{const gl=rt.gl;const mk=(t,s)=>{const o=gl.createShader(t);gl.shaderSource(o,s);gl.compileShader(o);if(!gl.getShaderParameter(o,gl.COMPILE_STATUS))throw new Error(gl.getShaderInfoLog(o));return o};const p=gl.createProgram();gl.attachShader(p,mk(gl.VERTEX_SHADER,vs));gl.attachShader(p,mk(gl.FRAGMENT_SHADER,fs));gl.linkProgram(p);if(!gl.getProgramParameter(p,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(p));gl.useProgram(p);['Globals','Obj','Mat'].forEach((n,i)=>{const bi=gl.getUniformBlockIndex(p,n);if(bi!==0xFFFFFFFF)gl.uniformBlockBinding(p,bi,i)});const names=texNames?texNames.split(';'):[];names.forEach((n,i)=>{const l=gl.getUniformLocation(p,'t_'+n);if(l)gl.uniform1i(l,i)});return rt.mats.push({prog:p,texCount:names.length,matSize:matSize})-1}"
prim__register : JSVal -> String -> String -> String -> Int -> PrimIO Int

||| The engine's line program, against the same blocks.
%foreign "javascript:lambda:(rt,vs,fs)=>{const gl=rt.gl;const mk=(t,s)=>{const o=gl.createShader(t);gl.shaderSource(o,s);gl.compileShader(o);if(!gl.getShaderParameter(o,gl.COMPILE_STATUS))throw new Error(gl.getShaderInfoLog(o));return o};const p=gl.createProgram();gl.attachShader(p,mk(gl.VERTEX_SHADER,vs));gl.attachShader(p,mk(gl.FRAGMENT_SHADER,fs));gl.linkProgram(p);if(!gl.getProgramParameter(p,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(p));gl.useProgram(p);['Globals','Obj'].forEach((n,i)=>{const bi=gl.getUniformBlockIndex(p,n);if(bi!==0xFFFFFFFF)gl.uniformBlockBinding(p,bi,i)});rt.lineProg=p;return 0}"
prim__lineInit : JSVal -> String -> String -> PrimIO Int

||| Decode an image URL (or data: URL) into an sRGB texture, linear
||| filtering, repeat wrap. Asynchronous: the continuation gets the table
||| index, or -1.
%foreign "javascript:lambda:(rt,url,k)=>{const gl=rt.gl;const img=new Image();img.onload=()=>{try{const t=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,t);gl.texImage2D(gl.TEXTURE_2D,0,gl.SRGB8_ALPHA8,gl.RGBA,gl.UNSIGNED_BYTE,img);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.LINEAR);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MAG_FILTER,gl.LINEAR);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_S,gl.REPEAT);gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_T,gl.REPEAT);k(rt.texs.push(t)-1)()}catch(e){k(-1)()}};img.onerror=()=>{k(-1)()};img.src=url;return 0}"
prim__loadTexture : JSVal -> String -> (Int -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(rt,a,n)=>{const gl=rt.gl;const b=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,b);gl.bufferData(gl.ARRAY_BUFFER,a,gl.STATIC_DRAW);return rt.meshes.push({b:b,n:n})-1}"
prim__createMesh : JSVal -> AnyPtr -> Int -> PrimIO Int

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
%foreign "javascript:lambda:(rt,mat,mesh,slot,t0,t1,t2,t3,blend,depth)=>{rt.all.push({k:blend?2:0,mat:mat,mesh:mesh,slot:slot,t0:t0,t1:t1,t2:t2,t3:t3,depth:depth});return 0}"
prim__draw : JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

||| The batched form: one foreign call records `count` consecutive slots.
%foreign "javascript:lambda:(rt,mat,mesh,first,count,t0,t1,t2,t3,blend,depth)=>{for(let i=0;i<count;i++)rt.all.push({k:blend?2:0,mat:mat,mesh:mesh,slot:first+i,t0:t0,t1:t1,t2:t2,t3:t3,depth:depth});return 0}"
prim__drawSlices : JSVal -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double -> PrimIO Int

||| A line-overlay draw, recorded (kind 1). Blended and depth-read-only at
||| replay; `lineWidth` is clamped to 1 by every shipping WebGL2
||| implementation, which is the width wanted here anyway.
%foreign "javascript:lambda:(rt,slot)=>{rt.all.push({k:1,slot:slot});return 0}"
prim__drawLines : JSVal -> Int -> PrimIO Int

||| The frame's replay: upload the filled prefix of both slot buffers once,
||| then the opaque phase in call order, the line overlays, and the
||| transparent phase back to front, blended and depth-read-only.
%foreign "javascript:lambda:(rt,count,stride)=>{const gl=rt.gl;const fl=count*(stride>>2);if(count>0){gl.bindBuffer(gl.UNIFORM_BUFFER,rt.obuf);gl.bufferSubData(gl.UNIFORM_BUFFER,0,rt.objArr,0,fl);gl.bindBuffer(gl.UNIFORM_BUFFER,rt.mbuf);gl.bufferSubData(gl.UNIFORM_BUFFER,0,rt.matArr,0,fl)}rt.curProg=null;rt.curMesh=null;rt.curTex=null;for(const d of rt.all)if(d.k===0)rt.execDraw(d);for(const d of rt.all)if(d.k===1)rt.execLine(d);const bl=rt.all.filter(d=>d.k===2);if(bl.length){bl.sort((a,b)=>b.depth-a.depth);gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);gl.depthMask(false);for(const d of bl)rt.execDraw(d);gl.depthMask(true);gl.disable(gl.BLEND)}rt.all.length=0;return 0}"
prim__flush : JSVal -> Int -> Int -> PrimIO Int

public export
record Gl2 where
  constructor MkGl2
  gl, canvas, rt : JSVal
  globalScratch : GlobalScratch
  objScratch, matScratch : ObjScratch
  ||| Eye and forward at `beginFrame`, for sorting the transparent phase.
  eyeFwd : IORef (V3, V3)
  lineCount : IORef Int

||| Build a WebGL2 renderer over the given canvas. Synchronous, unlike
||| WebGPU; materials arrive later, through `registerMaterial`.
export
initGl2 : (canvasId : String) -> IO Gl2
initGl2 canvasId = do
  canvas <- byId canvasId
  ignore (syncSize canvas)
  gl <- primIO (prim__context canvas)
  rt <- primIO (prim__initRt gl meshVertexSpec meshStride globalSize objSize
                             objStride maxObjects)
  ignore (primIO (prim__viewport gl))
  ignore (primIO (prim__lineInit rt (glslLineVert lineVertSrc)
                                 (glslLineFrag lineFragSrc)))
  gs <- newGlobalScratch
  os <- newObjScratch
  ms <- newObjScratch
  ignore (primIO (prim__attach rt (raw os) (raw ms)))
  ef <- newIORef (zero3, MkV3 0.0 0.0 (-1.0))
  MkGl2 gl canvas rt gs os ms ef <$> newIORef 0

||| WebGL2 keeps no pass object, so the token is pure capability plus the
||| slot counter: it exists to be threaded, and its constructor never leaves
||| this module.
export
data Gl2Frame : Type where
  MkGl2Frame : (nextSlot : Int) -> Gl2Frame

||| View-space depth of a model's translation, for the transparent sort.
depthOf : Gl2 -> Mat4 -> IO Double
depthOf r model = do
  (eye, fwd) <- readIORef r.eyeFwd
  pure (dot3 fwd (sub3 (MkV3 model.m12 model.m13 model.m14) eye))

export
Renderer Gl2 Gl2Frame where
  rendererName _ = "WebGL2"

  createMesh r vs =
    meshHandle <$> primIO (prim__createMesh r.rt (vertsRaw vs) (vertsCount vs))

  loadTexture r src k =
    let url = case src of
                FromPath p => p
                FromBase64 mime b64 => "data:" ++ mime ++ ";base64," ++ b64
     in ignore (primIO (prim__loadTexture r.rt url
          (\i => toPrim (k (if i < 0 then Nothing else Just (textureHandle i)) >> pure 0))))

  registerMaterial r {m} = do
    i <- primIO (prim__register r.rt (materialGlslVert {m}) (materialGlslFrag {m})
                                (materialTexNames {m}) (materialSize {m}))
    pure (materialId i)

  setLines r vs = do
    ignore (primIO (prim__setLines r.rt (vertsRaw vs) (vertsCount vs)))
    writeIORef r.lineCount (vertsCount vs)

  aspect r = primIO (prim__aspect r.gl)

  -- The drawing buffer is the viewport, so only the viewport needs redoing.
  resize r = do
    changed <- syncSize r.canvas
    when changed (ignore (primIO (prim__viewport r.gl)))

  beginFrame r cam lights t = do
    liftIO $ do
      ratio <- primIO (prim__aspect r.gl)
      pokeGlobals r.globalScratch cam ratio lights t False
      writeIORef r.eyeFwd
        (eyeOf cam, qRotate cam.transform.rotation (MkV3 0.0 0.0 (-1.0)))
      let cc = cam.clearColor
      ignore (primIO (prim__begin r.rt (raw r.globalScratch) globalFloats
                                  cc.red cc.green cc.blue))
    pure1 (Just (MkGl2Frame 0))

  draw r (MkGl2Frame i) mid mesh model v =
    case slot r.objScratch i of
      Nothing => pure1 (MkGl2Frame i)
      Just s => do
        liftIO $ do
          let am = alphaMode v
          pokeObject r.objScratch s model (alphaCode am) (alphaCutoff am) 0.0 0.0
          writeMat (matWriter r.matScratch s) v
          let (t0, t1, t2, t3) = texIds v
          d <- if isBlend am then depthOf r model else pure 0.0
          ignore (primIO (prim__draw r.rt (materialIdIndex mid) (meshIndex mesh)
                                     (slotIndex s) t0 t1 t2 t3
                                     (if isBlend am then 1 else 0) d))
        pure1 (MkGl2Frame (i + 1))

  drawMany r (MkGl2Frame i) mid mesh batch = do
    i' <- liftIO $ case batch of
      [] => pure i
      ((mdl0, v0) :: _) => do
        let am = alphaMode v0
            (t0, t1, t2, t3) = texIds v0
        filled <- fillBatch r.objScratch r.matScratch i batch
        let count = filled - i
        when (count > 0) $ do
          d <- if isBlend am then depthOf r mdl0 else pure 0.0
          ignore (primIO (prim__drawSlices r.rt (materialIdIndex mid)
                                           (meshIndex mesh) i count t0 t1 t2 t3
                                           (if isBlend am then 1 else 0) d))
        pure filled
    pure1 (MkGl2Frame i')

  drawLines r (MkGl2Frame i) colour =
    case slot r.objScratch i of
      Nothing => pure1 (MkGl2Frame i)
      Just s => do
        liftIO $ do
          n <- readIORef r.lineCount
          when (n > 0) $ do
            pokeObject r.objScratch s identity
                       colour.red colour.green colour.blue colour.alpha
            ignore (primIO (prim__drawLines r.rt (slotIndex s)))
        pure1 (MkGl2Frame (i + 1))

  -- Upload the two slot buffers once, then replay the recorded frame:
  -- opaque, lines, sorted transparency.
  endFrame r (MkGl2Frame i) =
    liftIO (ignore (primIO (prim__flush r.rt i objStride)))
