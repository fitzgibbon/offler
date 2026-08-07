||| The browser primitives offler needs: opaque JS handles, the DOM bits, and
||| requestAnimationFrame. Typed arrays are shared with the desktop build and
||| live in `Offler.Gfx.Array`.
|||
||| Every `%foreign` body here is one expression. That is deliberate: the
||| specifier is an Idris string literal, so it cannot contain a double quote
||| or a newline, and nothing in it is type-checked. Anything longer than a
||| line belongs in its own file.
module Offler.Web.Js

%default covering

||| An opaque JavaScript value: a context, a shader, a device, a DOM node.
public export
data JSVal : Type where [external]

--------------------------------------------------------------------------------
-- DOM

%foreign "javascript:lambda:(id)=>document.getElementById(id)"
prim__byId : String -> PrimIO JSVal

%foreign "javascript:lambda:(id,s)=>{const e=document.getElementById(id);if(e)e.textContent=s;return 0}"
prim__setText : String -> String -> PrimIO Int

%foreign "javascript:lambda:(id,f)=>{const e=document.getElementById(id);if(e)e.addEventListener('click',()=>{f(0)()});return 0}"
prim__onClick : String -> (Int -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(id)=>{const e=document.getElementById(id);if(e)e.disabled=true;return 0}"
prim__disable : String -> PrimIO Int

%foreign "javascript:lambda:(id)=>{const e=document.getElementById(id);return e?parseInt(e.value,10):0}"
prim__rangeValue : String -> PrimIO Int

%foreign "javascript:lambda:(id,v)=>{const e=document.getElementById(id);if(e)e.value=v;return 0}"
prim__setRangeValue : String -> Int -> PrimIO Int

%foreign "javascript:lambda:(id,f)=>{const e=document.getElementById(id);if(e)e.addEventListener('input',()=>{f(0)()});return 0}"
prim__onInput : String -> (Int -> PrimIO Int) -> PrimIO Int

||| Match a canvas's drawing buffer to its CSS box times the device pixel
||| ratio. Returns 1 when the size actually changed, so callers can skip
||| rebuilding size-dependent resources. Doing this only once at startup is
||| what makes a canvas look stretched after the window is resized: the
||| browser scales a stale buffer to fit the new box.
%foreign "javascript:lambda:(c)=>{const w=Math.max(1,Math.round(c.clientWidth*devicePixelRatio));const h=Math.max(1,Math.round(c.clientHeight*devicePixelRatio));if(c.width===w&&c.height===h)return 0;c.width=w;c.height=h;return 1}"
prim__syncSize : JSVal -> PrimIO Int

||| Run an action whenever the canvas's box changes. A ResizeObserver catches
||| layout changes as well as window resizes, and fires once when it starts
||| observing, which conveniently does the initial sizing too.
%foreign "javascript:lambda:(c,f)=>{new ResizeObserver(()=>{f(0)()}).observe(c);addEventListener('resize',()=>{f(0)()});return 0}"
prim__onResize : JSVal -> (Int -> PrimIO Int) -> PrimIO Int

export
byId : String -> IO JSVal
byId i = primIO (prim__byId i)

export
setText : String -> String -> IO ()
setText i s = ignore (primIO (prim__setText i s))

export
onClick : String -> IO () -> IO ()
onClick i act = ignore (primIO (prim__onClick i (\_ => toPrim (act >> pure 0))))

export
disable : String -> IO ()
disable i = ignore (primIO (prim__disable i))

export
rangeValue : String -> IO Int
rangeValue i = primIO (prim__rangeValue i)

export
setRangeValue : String -> Int -> IO ()
setRangeValue i v = ignore (primIO (prim__setRangeValue i v))

||| Fires continuously while a range input is dragged.
export
onInput : String -> IO () -> IO ()
onInput i act = ignore (primIO (prim__onInput i (\_ => toPrim (act >> pure 0))))

||| True when the drawing buffer had to change size.
export
syncSize : JSVal -> IO Bool
syncSize c = pure (!(primIO (prim__syncSize c)) /= 0)

export
onResize : JSVal -> IO () -> IO ()
onResize c act = ignore (primIO (prim__onResize c (\_ => toPrim (act >> pure 0))))

--------------------------------------------------------------------------------
-- Location and storage

%foreign "javascript:lambda:(k)=>{const v=new URLSearchParams(location.search).get(k);return v===null?'':v}"
prim__param : String -> PrimIO String

||| Reload with a new query string. Assigning `location.search` is not
||| reliable on a `file://` URL, so fall back to rebuilding the whole href.
%foreign "javascript:lambda:(q)=>{try{location.search=q}catch(e){location.href=location.href.split('?')[0]+q}return 0}"
prim__setSearch : String -> PrimIO Int

export
param : String -> IO String
param k = primIO (prim__param k)

export
setSearch : String -> IO ()
setSearch q = ignore (primIO (prim__setSearch q))

--------------------------------------------------------------------------------
-- Capability probe, frames, errors

%foreign "javascript:lambda:()=>navigator.gpu?1:0"
prim__hasWebGPU : PrimIO Int

export
hasWebGPU : IO Bool
hasWebGPU = pure (!(primIO prim__hasWebGPU) /= 0)

%foreign "javascript:lambda:(f)=>{requestAnimationFrame((t)=>{f(t*0.001)()});return 0}"
prim__nextFrame : (Double -> PrimIO Int) -> PrimIO Int

export
nextFrame : (Double -> IO ()) -> IO ()
nextFrame k = ignore (primIO (prim__nextFrame (\t => toPrim (k t >> pure 0))))

%foreign "javascript:lambda:(s)=>{const e=document.getElementById('error');if(e){e.style.display='block';e.textContent=s}return 0}"
prim__showError : String -> PrimIO Int

export
showError : String -> IO ()
showError s = ignore (primIO (prim__showError s))

||| Route uncaught errors and rejected promises to a handler. Worth installing
||| first: most of the startup path is asynchronous, so a failure inside a
||| promise would otherwise vanish silently and leave a blank canvas.
%foreign "javascript:lambda:(f)=>{addEventListener('error',e=>{f('Error: '+((e.error&&e.error.message)||e.message))()});addEventListener('unhandledrejection',e=>{f('Unhandled rejection: '+e.reason)()});return 0}"
prim__onUncaught : (String -> PrimIO Int) -> PrimIO Int

export
onUncaught : (String -> IO ()) -> IO ()
onUncaught k = ignore (primIO (prim__onUncaught (\s => toPrim (k s >> pure 0))))

--------------------------------------------------------------------------------
-- Input

||| Reports `KeyboardEvent.key`, so the caller does the matching.
%foreign "javascript:lambda:(f)=>{addEventListener('keydown',e=>{f(e.key)()});return 0}"
prim__onKeyDown : (String -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(f)=>{addEventListener('keyup',e=>{f(e.key)()});return 0}"
prim__onKeyUp : (String -> PrimIO Int) -> PrimIO Int

export
onKeyDown : (String -> IO ()) -> IO ()
onKeyDown k = ignore (primIO (prim__onKeyDown (\s => toPrim (k s >> pure 0))))

export
onKeyUp : (String -> IO ()) -> IO ()
onKeyUp k = ignore (primIO (prim__onKeyUp (\s => toPrim (k s >> pure 0))))

||| Pointer position in drawing-buffer pixels: CSS offset times the device
||| pixel ratio, matching what the native platform reports.
%foreign "javascript:lambda:(c,f)=>{c.addEventListener('pointermove',e=>{const r=c.getBoundingClientRect();f((e.clientX-r.left)*devicePixelRatio)((e.clientY-r.top)*devicePixelRatio)()});return 0}"
prim__onPointerMove : JSVal -> (Double -> Double -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(c,f)=>{c.addEventListener('pointerdown',e=>{const r=c.getBoundingClientRect();f(e.button)((e.clientX-r.left)*devicePixelRatio)((e.clientY-r.top)*devicePixelRatio)()});return 0}"
prim__onPointerDown : JSVal -> (Int -> Double -> Double -> PrimIO Int) -> PrimIO Int

%foreign "javascript:lambda:(c,f)=>{c.addEventListener('pointerup',e=>{const r=c.getBoundingClientRect();f(e.button)((e.clientX-r.left)*devicePixelRatio)((e.clientY-r.top)*devicePixelRatio)()});return 0}"
prim__onPointerUp : JSVal -> (Int -> Double -> Double -> PrimIO Int) -> PrimIO Int

||| Wheel, in lines-ish units, positive away from the user.
%foreign "javascript:lambda:(c,f)=>{c.addEventListener('wheel',e=>{f(-e.deltaY*0.01)()},{passive:true});return 0}"
prim__onWheel : JSVal -> (Double -> PrimIO Int) -> PrimIO Int

export
onPointerMove : JSVal -> (Double -> Double -> IO ()) -> IO ()
onPointerMove c k =
  ignore (primIO (prim__onPointerMove c (\x, y => toPrim (k x y >> pure 0))))

export
onPointerDown : JSVal -> (Int -> Double -> Double -> IO ()) -> IO ()
onPointerDown c k =
  ignore (primIO (prim__onPointerDown c (\b, x, y => toPrim (k b x y >> pure 0))))

export
onPointerUp : JSVal -> (Int -> Double -> Double -> IO ()) -> IO ()
onPointerUp c k =
  ignore (primIO (prim__onPointerUp c (\b, x, y => toPrim (k b x y >> pure 0))))

export
onWheel : JSVal -> (Double -> IO ()) -> IO ()
onWheel c k = ignore (primIO (prim__onWheel c (\d => toPrim (k d >> pure 0))))

--------------------------------------------------------------------------------
-- A growable store of foreign handles

||| Backends keep their per-mesh buffers in one of these: a plain JS array of
||| `{b, n}` records, so a `MeshHandle` is an index and lookup is O(1).
%foreign "javascript:lambda:()=>[]"
prim__newStore : PrimIO JSVal

%foreign "javascript:lambda:(s,b,n)=>s.push({b:b,n:n})-1"
prim__storeAdd : JSVal -> JSVal -> Int -> PrimIO Int

%foreign "javascript:lambda:(s,i)=>s[i]?s[i].b:null"
prim__storeBuf : JSVal -> Int -> PrimIO JSVal

%foreign "javascript:lambda:(s,i)=>s[i]?s[i].n:0"
prim__storeCount : JSVal -> Int -> PrimIO Int

export
newStore : IO JSVal
newStore = primIO prim__newStore

export
storeAdd : JSVal -> JSVal -> Int -> IO Int
storeAdd s b n = primIO (prim__storeAdd s b n)

export
storeBuf : JSVal -> Int -> IO JSVal
storeBuf s i = primIO (prim__storeBuf s i)

export
storeCount : JSVal -> Int -> IO Int
storeCount s i = primIO (prim__storeCount s i)
