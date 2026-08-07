||| `Offler.Gfx.Platform` on SDL3.
|||
||| The desktop owns its loop, so `runLoop` is an ordinary tail-recursive
||| loop that ends when the window closes -- the mirror of the browser's
||| requestAnimationFrame chain. There being no status bar, the status slots
||| are composed into the window title.
|||
||| `Escape` closes the window, in addition to being delivered as a key
||| event, so every example quits the same way without wiring it themselves.
module Offler.Native.Platform

import Data.IORef
import Data.List
import Offler.Gfx.Platform

%default covering

%foreign "C:offler_poll,liboffler"
prim__poll : AnyPtr -> PrimIO Int

%foreign "C:offler_event_key,liboffler"
prim__eventKey : AnyPtr -> PrimIO String

%foreign "C:offler_event_x,liboffler"
prim__eventX : AnyPtr -> PrimIO Double

%foreign "C:offler_event_y,liboffler"
prim__eventY : AnyPtr -> PrimIO Double

%foreign "C:offler_event_wheel,liboffler"
prim__eventWheel : AnyPtr -> PrimIO Double

%foreign "C:offler_event_button,liboffler"
prim__eventButton : AnyPtr -> PrimIO Int

%foreign "C:offler_time,liboffler"
prim__time : AnyPtr -> PrimIO Double

%foreign "C:offler_set_title,liboffler"
prim__setTitle : AnyPtr -> String -> PrimIO ()

%foreign "C:offler_quit,liboffler"
prim__quit : AnyPtr -> PrimIO ()

public export
record NativePlatform where
  constructor MkNative
  ctx : AnyPtr
  ||| Composed into the window title, before the status slots.
  appName : String
  running : IORef Bool
  slots : IORef (List (String, String))

export
initNative : (appName : String) -> AnyPtr -> IO NativePlatform
initNative name c = MkNative c name <$> newIORef True <*> newIORef []

||| Fixed order, so the title does not reshuffle itself as slots are set.
titleOrder : List String
titleOrder = ["backend", "count-label", "stats", "fps", "note"]

composeTitle : String -> List (String, String) -> String
composeTitle name ss =
  name ++ concatMap piece titleOrder
  where
    piece : String -> String
    piece k = case lookup k ss of
                Nothing => ""
                Just v => "  |  " ++ v

buttonOf : Int -> Button
buttonOf 0 = LeftButton
buttonOf 1 = MiddleButton
buttonOf 2 = RightButton
buttonOf n = OtherButton n

||| Drain SDL's queue into offler's vocabulary. A close code stops the loop
||| after delivering `CloseRequested`; so does `Escape`.
drain : NativePlatform -> List Event -> IO (List Event)
drain p acc = do
  code <- primIO (prim__poll p.ctx)
  case code of
    0 => pure (reverse acc)
    1 => do writeIORef p.running False
            pure (reverse (CloseRequested :: acc))
    2 => drain p (Resized :: acc)
    3 => do k <- primIO (prim__eventKey p.ctx)
            when (k == "Escape") (writeIORef p.running False)
            drain p (KeyDown k :: acc)
    4 => do k <- primIO (prim__eventKey p.ctx)
            drain p (KeyUp k :: acc)
    5 => do x <- primIO (prim__eventX p.ctx)
            y <- primIO (prim__eventY p.ctx)
            drain p (PointerMove x y :: acc)
    6 => do b <- primIO (prim__eventButton p.ctx)
            x <- primIO (prim__eventX p.ctx)
            y <- primIO (prim__eventY p.ctx)
            drain p (PointerDown (buttonOf b) x y :: acc)
    7 => do b <- primIO (prim__eventButton p.ctx)
            x <- primIO (prim__eventX p.ctx)
            y <- primIO (prim__eventY p.ctx)
            drain p (PointerUp (buttonOf b) x y :: acc)
    8 => do d <- primIO (prim__eventWheel p.ctx)
            drain p (Wheel d :: acc)
    _ => drain p acc

loop : NativePlatform -> (Double -> IO ()) -> IO ()
loop p k = do
  go <- readIORef p.running
  if not go
    then primIO (prim__quit p.ctx)
    else do
      t <- primIO (prim__time p.ctx)
      k t
      loop p k

export
Platform NativePlatform where
  runLoop p k = loop p k

  pollEvents p = drain p []

  setStatus p slot s = do
    ss <- readIORef p.slots
    let ss' = (slot, s) :: filter ((/= slot) . fst) ss
    writeIORef p.slots ss'
    primIO (prim__setTitle p.ctx (composeTitle p.appName ss'))
