||| Events between systems: bevy's `Events<E>` with its `EventReader`.
|||
||| `Events e` is a resource holding two buffers, this frame's and the
||| previous frame's, each event stamped with the tick it was sent at. A
||| reader sees the events sent since *it* last ran, so every system sees
||| every event exactly once wherever it sits in the order -- a system
||| earlier in the frame than the sender sees it next frame -- and an event
||| lives for two frames at most.
module Offler.Ecs.Event

import Offler.Ecs.World

%default total

public export
record Events e where
  constructor MkEvents
  ||| The frame-start tick `recent` was begun at.
  epoch : Int
  ||| This frame's events, newest first.
  recent : List (Int, e)
  ||| The previous frame's, newest first.
  older : List (Int, e)

export
Resource (Events e) where
  initial = MkEvents 0 [] []

||| The buffers as they stand in the frame that began at `start`, the
||| previous one at `floor`: a buffer from an earlier frame moves to
||| `older` or is dropped.
current : (start, floor : Int) -> Events e -> Events e
current start floor ev =
  if ev.epoch == start
    then ev
    else MkEvents start [] (if ev.epoch == floor then ev.recent else [])

||| Send an event, stamped with the current tick.
export
sendEvent : {auto el : Has (Events e) cs} -> World cs -> e -> IO ()
sendEvent w ev = do
  t <- currentTick w
  (start, floor) <- frameTicks w
  modifyResource w $ \evs =>
    let MkEvents ep r o = current start floor evs in MkEvents ep ((t, ev) :: r) o

||| Send several, in order.
export
sendEvents : {auto el : Has (Events e) cs} -> World cs -> List e -> IO ()
sendEvents w = traverse_ (sendEvent w)

||| The events sent since the running system last ran, oldest first.
export
readEvents : (0 e : Type) -> {auto el : Has (Events e) cs} -> World cs -> IO (List e)
readEvents _ w = do
  seen <- seenTick w
  (start, floor) <- frameTicks w
  MkEvents _ r o <- current start floor <$> resource w
  pure (map snd (reverse (filter (newer seen) r ++ filter (newer seen) o)))
  where
    newer : Int -> (Int, e) -> Bool
    newer seen (t, _) = t > seen
