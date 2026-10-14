||| Systems in order, and the frame time they run against: bevy's `Update`
||| schedule and `Res<Time>`, reduced to what a single-threaded frame loop
||| needs. The order written is the order run.
|||
||| A schedule remembers the tick each system last ran at, which is what
||| `Changed`, `Added` and `readEvents` mean by "since": each system sees
||| what changed since *it* last ran, as in bevy.
|||
||| Schedules and clocks live outside the world they run against, so a
||| system closing over the world never ends up stored inside it.
module Offler.Ecs.Schedule

import Data.IORef
import Offler.Ecs.World

%default covering

public export
record Tick where
  constructor MkTick
  ||| Seconds since the platform's clock started.
  time : Double
  ||| Seconds since the previous tick.
  delta : Double

public export
System : List Type -> Type
System cs = Tick -> World cs -> IO ()

export
record Schedule (cs : List Type) where
  constructor MkSchedule
  systems : List (System cs, IORef Int)

export
newSchedule : List (System cs) -> IO (Schedule cs)
newSchedule ss = MkSchedule <$> traverse (\s => MkPair s <$> newIORef 0) ss

||| Run one frame: every system once, in order, each against the changes
||| since its own previous run.
export
runSchedule : Schedule cs -> Tick -> World cs -> IO ()
runSchedule sch t w = do
  startFrame w
  traverse_ step sch.systems
  where
    step : (System cs, IORef Int) -> IO ()
    step (sys, last) = do
      seen <- readIORef last
      now <- enterSystem w seen
      sys t w
      writeIORef last now
      leaveSystem w

||| Turns the platform loop's timestamps into ticks.
export
record Clock where
  constructor MkClock
  last : IORef (Maybe Double)
  maxDelta : Double

||| A clock whose deltas never exceed `maxDelta` -- a backgrounded tab or a
||| stalled window resumes with one ordinary step, not a leap.
export
newClock : (maxDelta : Double) -> IO Clock
newClock m = (\r => MkClock r m) <$> newIORef Nothing

export
tick : Clock -> (time : Double) -> IO Tick
tick c t = do
  prev <- readIORef c.last
  writeIORef c.last (Just t)
  pure (MkTick t (maybe 0.0 (\p => max 0.0 (min c.maxDelta (t - p))) prev))
