||| WebGL2 implementation of `Renderer`.
module Offler.Web.Gl2

import Data.IORef
import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math
import Offler.Web.Js

-- `Control.Linear.LIO`, re-exported by Offler.Gfx.Renderer, defines
-- `fromInteger` for its `Usage` type, which makes bare integer literals
-- ambiguous.
%hide Control.Linear.LIO.fromInteger

%default covering

%foreign "javascript:lambda:(c)=>c.getContext('webgl2',{antialias:true})"
prim__context : JSVal -> PrimIO JSVal

%foreign "javascript:lambda:(gl)=>{gl.enable(gl.DEPTH_TEST);gl.enable(gl.CULL_FACE);return 0}"
prim__setup : JSVal -> PrimIO Int

%foreign "javascript:lambda:(gl)=>{gl.viewport(0,0,gl.drawingBufferWidth,gl.drawingBufferHeight);return 0}"
prim__viewport : JSVal -> PrimIO Int

||| Compile, link and use. Throws with the driver's log if either step fails,
||| which the page's error box then shows.
%foreign "javascript:lambda:(gl,vs,fs)=>{const mk=(t,s)=>{const o=gl.createShader(t);gl.shaderSource(o,s);gl.compileShader(o);if(!gl.getShaderParameter(o,gl.COMPILE_STATUS))throw new Error(gl.getShaderInfoLog(o));return o};const p=gl.createProgram();gl.attachShader(p,mk(gl.VERTEX_SHADER,vs));gl.attachShader(p,mk(gl.FRAGMENT_SHADER,fs));gl.linkProgram(p);if(!gl.getProgramParameter(p,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(p));gl.useProgram(p);return p}"
prim__program : JSVal -> String -> String -> PrimIO JSVal

%foreign "javascript:lambda:(gl,p,n)=>gl.getUniformLocation(p,n)"
prim__uniform : JSVal -> JSVal -> String -> PrimIO JSVal

%foreign "javascript:lambda:(gl)=>gl.drawingBufferWidth/gl.drawingBufferHeight"
prim__aspect : JSVal -> PrimIO Double

%foreign "javascript:lambda:(gl,loc,a)=>{gl.uniformMatrix4fv(loc,false,a);return 0}"
prim__mat4 : JSVal -> JSVal -> AnyPtr -> PrimIO Int

%foreign "javascript:lambda:(gl,loc,x,y,z)=>{gl.uniform3f(loc,x,y,z);return 0}"
prim__vec3 : JSVal -> JSVal -> Double -> Double -> Double -> PrimIO Int

%foreign "javascript:lambda:(gl,loc,x,y,z,w)=>{gl.uniform4f(loc,x,y,z,w);return 0}"
prim__vec4 : JSVal -> JSVal -> Double -> Double -> Double -> Double -> PrimIO Int

%foreign "javascript:lambda:(gl,loc,x)=>{gl.uniform1f(loc,x);return 0}"
prim__float : JSVal -> JSVal -> Double -> PrimIO Int

%foreign "javascript:lambda:(gl,r,g,b)=>{gl.clearColor(r,g,b,1);gl.clear(gl.COLOR_BUFFER_BIT|gl.DEPTH_BUFFER_BIT);return 0}"
prim__clear : JSVal -> Double -> Double -> Double -> PrimIO Int

%foreign "javascript:lambda:(gl,n)=>{gl.drawArrays(gl.TRIANGLES,0,n);return 0}"
prim__draw : JSVal -> Int -> PrimIO Int

%foreign "javascript:lambda:(gl)=>gl.createBuffer()"
prim__makeBuffer : JSVal -> PrimIO JSVal

||| Point the attributes at a buffer, from a `Offler.Gfx.Layout` vertex spec:
||| one `location,byteOffset,components` triple per attribute.
%foreign "javascript:lambda:(gl,b,spec,stride)=>{gl.bindBuffer(gl.ARRAY_BUFFER,b);spec.split(';').forEach(e=>{const a=e.split(',').map(Number);gl.enableVertexAttribArray(a[0]);gl.vertexAttribPointer(a[0],a[2],gl.FLOAT,false,stride,a[1])});return 0}"
prim__attrib : JSVal -> JSVal -> String -> Int -> PrimIO Int

||| Turn a vertex attribute into a constant: what the line overlay does with
||| the normal, which its buffer does not carry.
%foreign "javascript:lambda:(gl,loc,x,y,z)=>{gl.disableVertexAttribArray(loc);gl.vertexAttrib3f(loc,x,y,z);return 0}"
prim__constAttrib : JSVal -> Int -> Double -> Double -> Double -> PrimIO Int

%foreign "javascript:lambda:(gl,b,data)=>{gl.bindBuffer(gl.ARRAY_BUFFER,b);gl.bufferData(gl.ARRAY_BUFFER,data,gl.STATIC_DRAW);return 0}"
prim__upload : JSVal -> JSVal -> AnyPtr -> PrimIO Int

||| Blended, and with depth writes off so lines do not stipple each other
||| where they cross. `lineWidth` is clamped to 1 by every shipping WebGL2
||| implementation, which is the width wanted here anyway.
%foreign "javascript:lambda:(gl,n)=>{gl.enable(gl.BLEND);gl.blendFunc(gl.SRC_ALPHA,gl.ONE_MINUS_SRC_ALPHA);gl.depthMask(false);gl.drawArrays(gl.LINES,0,n);gl.depthMask(true);gl.disable(gl.BLEND);return 0}"
prim__drawLines : JSVal -> Int -> PrimIO Int

public export
record Gl2 where
  constructor MkGl2
  gl, canvas : JSVal
  ||| The mesh table: a `MeshHandle` indexes it.
  meshes : JSVal
  lineBuf : JSVal
  uProj, uView, uModel, uCam, uTime : JSVal
  uLightDir, uAmbient, uLightColor : JSVal
  uBaseColor, uEmissive, uParams : JSVal
  ||| One matrix at a time: WebGL2 sets uniforms individually, so this never
  ||| needs to hold more than the sixteen floats `uniformMatrix4fv` reads.
  scratch : F32Array 16
  lineCount : IORef Int

||| Build a WebGL2 renderer over the given canvas. Synchronous, unlike WebGPU.
export
initGl2 : (canvasId : String) -> (vertSrc : String) -> (fragSrc : String) -> IO Gl2
initGl2 canvasId vertSrc fragSrc = do
  canvas <- byId canvasId
  ignore (syncSize canvas)
  gl <- primIO (prim__context canvas)
  -- The attribute declarations the pointers below were derived from, ahead
  -- of the authored body but behind the version directive.
  prog <- primIO (prim__program gl (withGlslPrologue vertSrc) fragSrc)
  ignore (primIO (prim__setup gl))
  ignore (primIO (prim__viewport gl))
  meshes <- newStore
  lbuf <- primIO (prim__makeBuffer gl)
  let u : String -> IO JSVal
      u n = primIO (prim__uniform gl prog n)
  MkGl2 gl canvas meshes lbuf
    <$> u "uProj" <*> u "uView" <*> u "uModel" <*> u "uCam" <*> u "uTime"
    <*> u "uLightDir" <*> u "uAmbient" <*> u "uLightColor"
    <*> u "uBaseColor" <*> u "uEmissive" <*> u "uParams"
    <*> newF32 16
    <*> newIORef 0

||| WebGL2 keeps no per-frame state -- uniforms take effect immediately and
||| there is no pass object -- so the token is pure capability: it exists to
||| be threaded, and its constructor never leaves this module.
export
data Gl2Frame : Type where
  MkGl2Frame : Gl2Frame

||| Object uniforms, in plain `IO`: the batched draw runs this in a loop
||| inside one lifted action.
setObj : Gl2 -> Mat4 -> Material -> IO ()
setObj r model mat = do
  pokeMat r.scratch (here 0) model
  ignore (primIO (prim__mat4 r.gl r.uModel (raw r.scratch)))
  let bc = mat.baseColor
      em = mat.emissive
  ignore (primIO (prim__vec4 r.gl r.uBaseColor bc.red bc.green bc.blue bc.alpha))
  ignore (primIO (prim__vec4 r.gl r.uEmissive em.red em.green em.blue (modeCode mat)))
  ignore (primIO (prim__vec4 r.gl r.uParams mat.metallic mat.roughness 0.0 0.0))

||| Bind a mesh's buffer to the attributes and return its vertex count.
bindMesh : Gl2 -> MeshHandle -> IO Int
bindMesh r m = do
  buf <- storeBuf r.meshes (meshIndex m)
  ignore (primIO (prim__attrib r.gl buf meshVertexSpec meshStride))
  storeCount r.meshes (meshIndex m)

drawAllGl2 : Gl2 -> Int -> List (Mat4, Material) -> IO ()
drawAllGl2 _ _ [] = pure ()
drawAllGl2 r n ((m, mt) :: rest) = do
  setObj r m mt
  ignore (primIO (prim__draw r.gl n))
  drawAllGl2 r n rest

export
Renderer Gl2 Gl2Frame where
  rendererName _ = "WebGL2"

  createMesh r vs = do
    buf <- primIO (prim__makeBuffer r.gl)
    ignore (primIO (prim__upload r.gl buf (vertsRaw vs)))
    meshHandle <$> storeAdd r.meshes buf (vertsCount vs)

  setLines r vs = do
    ignore (primIO (prim__upload r.gl r.lineBuf (vertsRaw vs)))
    writeIORef r.lineCount (vertsCount vs)

  aspect r = primIO (prim__aspect r.gl)

  -- The drawing buffer is the viewport, so only the viewport needs redoing.
  resize r = do
    changed <- syncSize r.canvas
    when changed (ignore (primIO (prim__viewport r.gl)))

  beginFrame r cam lights t = do
    liftIO $ do
      ratio <- primIO (prim__aspect r.gl)
      pokeMat r.scratch (here 0) (projMatrix cam.projection ratio)
      ignore (primIO (prim__mat4 r.gl r.uProj (raw r.scratch)))
      pokeMat r.scratch (here 0) (viewMatrix cam)
      ignore (primIO (prim__mat4 r.gl r.uView (raw r.scratch)))
      let eye = eyeOf cam
          dir = normalize3 lights.direction
          lc = lights.color
          cc = cam.clearColor
      ignore (primIO (prim__vec3 r.gl r.uCam eye.vx eye.vy eye.vz))
      ignore (primIO (prim__float r.gl r.uTime t))
      ignore (primIO (prim__vec3 r.gl r.uLightDir dir.vx dir.vy dir.vz))
      ignore (primIO (prim__float r.gl r.uAmbient lights.ambient))
      ignore (primIO (prim__vec3 r.gl r.uLightColor lc.red lc.green lc.blue))
      ignore (primIO (prim__clear r.gl cc.red cc.green cc.blue))
    pure1 (Just MkGl2Frame)

  draw r MkGl2Frame m model mat = do
    liftIO $ do
      n <- bindMesh r m
      setObj r model mat
      ignore (primIO (prim__draw r.gl n))
    pure1 MkGl2Frame

  drawMany r MkGl2Frame m batch = do
    liftIO $ do
      n <- bindMesh r m
      drawAllGl2 r n batch
    pure1 MkGl2Frame

  -- Topology is an argument to the draw here rather than baked into a
  -- pipeline, so the overlay reuses the one program: point the attributes at
  -- the line buffer, pin the normal to a constant, draw, and the next mesh
  -- draw points them back.
  drawLines r MkGl2Frame mat = do
    liftIO $ do
      n <- readIORef r.lineCount
      when (n > 0) $ do
        setObj r identity ({ unlit := True } mat)
        ignore (primIO (prim__attrib r.gl r.lineBuf lineVertexSpec lineStride))
        ignore (primIO (prim__constAttrib r.gl 1 0.0 1.0 0.0))
        ignore (primIO (prim__drawLines r.gl n))
    pure1 MkGl2Frame

  endFrame _ MkGl2Frame = pure ()
