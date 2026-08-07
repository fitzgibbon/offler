||| The window, the clock and the input, abstracted over browser and desktop.
|||
||| The awkward difference between the two is who owns the loop: the browser
||| hands you a callback, a desktop app runs its own. `runLoop` absorbs that,
||| so a scene never has to know which world it is in.
module Offler.Gfx.Platform

%default covering

public export
data Button = LeftButton | MiddleButton | RightButton | OtherButton Int

public export
Eq Button where
  LeftButton == LeftButton = True
  MiddleButton == MiddleButton = True
  RightButton == RightButton = True
  OtherButton a == OtherButton b = a == b
  _ == _ = False

||| Input, in the browser's vocabulary. Key names follow `KeyboardEvent.key`
||| ("a", "Escape", "ArrowLeft", " ", "+"); the native platform translates
||| SDL's names to match, so a scene matches on one spelling.
public export
data Event
  = ||| The drawing surface changed size.
    Resized
  | ||| The user asked the window to close. Browser platforms never send it;
    ||| the native platform stops its loop after delivering it.
    CloseRequested
  | KeyDown String
  | KeyUp String
  | ||| Pointer position in drawing-surface pixels, origin top-left.
    PointerMove Double Double
  | PointerDown Button Double Double
  | PointerUp Button Double Double
  | ||| Positive away from the user, in lines-ish units.
    Wheel Double

public export
interface Platform p where
  ||| Drive frames until the platform decides to stop, passing seconds
  ||| elapsed. Never returns in the browser; returns on window close
  ||| natively.
  runLoop : p -> (Double -> IO ()) -> IO ()

  ||| Drain input since the last call. Browser listeners queue into a ref;
  ||| natively this pumps the event loop. Called once per frame, on the frame
  ||| thread, so no state changes from a callback.
  pollEvents : p -> IO (List Event)
