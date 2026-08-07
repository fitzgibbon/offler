||| The window, the clock and the input, abstracted over browser and desktop.
|||
||| The awkward difference between the two is who owns the loop: the browser
||| hands you a callback, a desktop app runs its own. `runLoop` absorbs that,
||| so a scene never has to know which world it is in.
|||
||| Input follows the same rule. Mice and keyboards arrive as `Event`s in one
||| vocabulary; fingers arrive as `Touch*` events and never masquerade as
||| pointers (both backends filter the compatibility mouse events their
||| platform synthesises); gamepads are *polled*, because that is the only
||| model the Gamepad API offers and SDL's event stream is easily sampled
||| into it.
module Offler.Gfx.Platform

import Data.List1
import Data.String

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
  | ||| Relative pointer motion in drawing-surface pixels. Delivered beside
    ||| every `PointerMove`; under pointer lock it is the only motion still
    ||| delivered, since an absolute position would be pinned to the centre.
    PointerDelta Double Double
  | PointerDown Button Double Double
  | PointerUp Button Double Double
  | ||| Positive away from the user, in lines-ish units.
    Wheel Double
  | ||| A finger went down: its id (stable while the finger stays down) and
    ||| position in drawing-surface pixels.
    TouchStart Int Double Double
  | TouchMove Int Double Double
  | ||| The finger lifted, or the platform cancelled the gesture; either way
    ||| the id is done.
    TouchEnd Int Double Double
  | ||| A pad appeared: its index (the key for `gamepads`) and name.
    GamepadConnected Int String
  | GamepadDisconnected Int
  | ||| The pointer-lock state actually changed. The platform is the source
    ||| of truth, not the request: browsers grant `setPointerLock` later or
    ||| not at all, and release it themselves on `Escape`, so a scene that
    ||| mirrored its own requests would drift. Track this event instead.
    PointerLockChanged Bool

||| The standard-mapping buttons both worlds agree on: the Gamepad API's
||| standard layout and SDL's `SDL_GamepadButton`, which name the same
||| physical pad. `South` is the bottom face button -- A on an Xbox pad,
||| Cross on a DualShock.
public export
data GamepadButton
  = South | East | West | North
  | L1 | R1
  | Select | Start
  | LStick | RStick
  | DPadUp | DPadDown | DPadLeft | DPadRight

||| Also the bit each button occupies in the wire mask both backends emit.
public export
buttonBit : GamepadButton -> Nat
buttonBit South = 0
buttonBit East = 1
buttonBit West = 2
buttonBit North = 3
buttonBit L1 = 4
buttonBit R1 = 5
buttonBit Select = 6
buttonBit Start = 7
buttonBit LStick = 8
buttonBit RStick = 9
buttonBit DPadUp = 10
buttonBit DPadDown = 11
buttonBit DPadLeft = 12
buttonBit DPadRight = 13

public export
Eq GamepadButton where
  a == b = buttonBit a == buttonBit b

allButtons : List GamepadButton
allButtons = [South, East, West, North, L1, R1, Select, Start,
              LStick, RStick, DPadUp, DPadDown, DPadLeft, DPadRight]

||| One pad's state, as sampled by `gamepads`. Sticks are -1..1 (right and
||| down positive, unfiltered -- dead zones are the scene's policy), triggers
||| 0..1, and `buttons` is the set currently held.
public export
record Gamepad where
  constructor MkGamepad
  index : Int
  name : String
  leftX, leftY, rightX, rightY : Double
  leftTrigger, rightTrigger : Double
  buttons : List GamepadButton

export
pressed : GamepadButton -> Gamepad -> Bool
pressed b g = elem b g.buttons

||| Decode the wire form both backends emit: records semicolon-separated,
||| the first eight fields comma-separated numbers, the name the (possibly
||| comma-containing) remainder. The same shared-string convention as the
||| renderer's layout specs -- one parser, two producers.
export
parseGamepads : String -> List Gamepad
parseGamepads s = mapMaybe padOf (forget (split (== ';') s))
  where
    num : String -> Double
    num = cast

    bit : Nat -> Int
    bit Z = 1
    bit (S k) = 2 * bit k

    isSet : Int -> Nat -> Bool
    isSet m k = mod (div m (bit k)) 2 == 1

    padOf : String -> Maybe Gamepad
    padOf rec = case forget (split (== ',') rec) of
      (i :: lx :: ly :: rx :: ry :: lt :: rt :: mask :: nameParts) =>
        let m = the Int (cast mask) in
        Just (MkGamepad (cast i) (joinBy "," nameParts)
                (num lx) (num ly) (num rx) (num ry)
                (num lt) (num rt)
                (filter (\b => isSet m (buttonBit b)) allButtons))
      _ => Nothing

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

  ||| Sample every connected pad. Polling is the model both worlds share:
  ||| the browser only exposes snapshots, and SDL's stream samples into one.
  gamepads : p -> IO (List Gamepad)

  ||| *Request* capturing the pointer for relative motion: the cursor
  ||| disappears, `PointerMove` stops (there is no meaningful position),
  ||| `PointerDelta` keeps arriving. This is only the request -- the state
  ||| changes when `PointerLockChanged` says so. Browsers grant it only
  ||| shortly after a real user gesture (ask from a click or key handler's
  ||| frame), may decline silently (no event arrives), and release it
  ||| themselves on `Escape`; SDL grants synchronously and the event
  ||| arrives on the next poll.
  setPointerLock : p -> Bool -> IO ()

  ||| Hide or show the cursor without capturing it.
  setCursorVisible : p -> Bool -> IO ()

  ||| Drawing-surface size in pixels, as it is now -- the space every
  ||| pointer and touch coordinate is reported in.
  surfaceSize : p -> IO (Double, Double)
