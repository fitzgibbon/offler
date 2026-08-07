||| `Offler.Gfx.Platform` for the browser: requestAnimationFrame drives the
||| loop, and DOM listeners queue events for the scene to drain.
module Offler.Web.Platform

import Data.IORef
import Offler.Gfx.Platform
import Offler.Web.Js

%default covering

public export
record WebPlatform where
  constructor MkWeb
  canvas : JSVal
  queue : IORef (List Event)

buttonOf : Int -> Button
buttonOf 0 = LeftButton
buttonOf 1 = MiddleButton
buttonOf 2 = RightButton
buttonOf n = OtherButton n

||| Listeners fire whenever the browser feels like it, so they push onto a
||| queue rather than touching the scene directly. That keeps every state
||| change on the frame thread, exactly as the desktop build works.
export
initWeb : (canvasId : String) -> IO WebPlatform
initWeb canvasId = do
  c <- byId canvasId
  q <- newIORef []
  let push : Event -> IO ()
      push e = modifyIORef q (e ::)
  onResize c (push Resized)
  onKeyDown (push . KeyDown)
  onKeyUp (push . KeyUp)
  onPointerMove c (\x, y => push (PointerMove x y))
  onPointerDown c (\b, x, y => push (PointerDown (buttonOf b) x y))
  onPointerUp c (\b, x, y => push (PointerUp (buttonOf b) x y))
  onWheel c (push . Wheel)
  pure (MkWeb c q)

webLoop : (Double -> IO ()) -> Double -> IO ()
webLoop k t = k t >> nextFrame (webLoop k)

export
Platform WebPlatform where
  runLoop _ k = nextFrame (webLoop k)

  pollEvents p = do
    es <- readIORef p.queue
    writeIORef p.queue []
    pure (reverse es)

||| The example pages' status line: a DOM element by id. Deliberately not a
||| `Platform` member -- it is page chrome, not a windowing concept -- so
||| scenes take it as a plain function from their `main`.
export
webStatus : (slot : String) -> String -> IO ()
webStatus = setText
