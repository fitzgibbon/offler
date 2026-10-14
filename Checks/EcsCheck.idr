||| Runs the ECS's query loops at a capacity far past where an
||| untrampolined loop overflows V8, and checks what they compute: packed,
||| boxed and marker stores, joins, optional and excluded components,
||| removal and despawn *during* iteration, and index reuse by generation.
module Checks.EcsCheck

import Data.IORef
import Data.Maybe
import Data.String
import Offler.Camera
import Offler.Ecs

%default covering

record Pos where
  constructor MkPos
  px : Double

record Vel where
  constructor MkVel
  vx : Double

record Name where
  constructor MkName
  name : String

data Odd = MkOdd

record Gravity where
  constructor MkGravity
  grav : Double

Resource Gravity where
  initial = MkGravity 9.0

Columnar Pos where
  width = 1
  storeCol a o (MkPos x) = poke a o x
  loadCol a o = MkPos <$> peek a o

Columnar Vel where
  width = 1
  storeCol a o (MkVel x) = poke a o x
  loadCol a o = MkVel <$> peek a o

Component Pos where kind = Packed
Component Vel where kind = Packed
Component Name where kind = Boxed
Component Odd where kind = Marker MkOdd

W : List Type
W = [Pos, Vel, Name, Odd, Gravity]

cap : Int
cap = 100000

fill : World W -> Int -> IO ()
fill w i =
  if i >= cap then pure () else do
    _ <- if i `mod` 2 == 1
           then spawn w (MkPos (cast i), MkVel 1.0, MkOdd)
           else spawn w (MkPos (cast i), MkVel 1.0)
    fill w (i + 1)

spawnOdd : World W -> Int -> IO ()
spawnOdd w k = if k <= 0 then pure () else ignore (spawn w MkOdd) >> spawnOdd w (k - 1)

step : (Pos, Vel) -> Pos
step (MkPos p, MkVel v) = MkPos (p + v)

total' : World W -> IO Double
total' w = cfold {q = Pos} w (\s, p => s + p.px) 0.0

||| A small world run through a schedule for four frames: what each
||| system sees of changes and events depends on where it sits relative to
||| the writer.
V : List Type
V = [Pos, Events Int]

record Log where
  constructor MkLog
  frame : IORef Int
  lines : IORef (List String)

say : Log -> String -> IO ()
say l x = modifyIORef l.lines (x ::)

counter : String -> Log -> System V
counter tag l _ w = do
  ch <- cfold {q = Changed Pos} w (\k, _ => k + 1) (the Int 0)
  ad <- cfold {q = Added Pos} w (\k, _ => k + 1) (the Int 0)
  evs <- readEvents Int w
  say l (tag ++ show ch ++ "/" ++ show ad ++ show evs)

writer : Log -> System V
writer l _ w = do
  f <- readIORef l.frame
  when (f == 2) $ cfor {q = (Entity, Pos)} w $ \(e, MkPos p) =>
    when (entityIndex e < 3) $ insert w e (MkPos (p + 1.0))
  when (f <= 2) $ sendEvent w f

||| A pickable child under a moved parent, picked through the camera at the
||| centre of the screen and missed at the edge: `pickTargets` places
||| bounds by the propagated `GlobalTransform`.
P : List Type
P = [Transform, GlobalTransform, ChildOf, Pickable]

picked : IO String
picked = do
  v <- newWorld {cs = P} 8
  Just root <- spawn v (at (v3 0.0 0.0 (-5.0))) | Nothing => pure "spawn failed"
  Just kid <- spawn v (at (v3 0.0 0.0 (-5.0)), MkChildOf root, MkPickable (BoundSphere zero3 1.0))
    | Nothing => pure "spawn failed"
  propagate v
  ts <- pickTargets v
  let cam = perspectiveCamera neutral
      hit = map fst (pickAt cam (100.0, 100.0) ts 50.0 50.0)
      miss = map fst (pickAt cam (100.0, 100.0) ts 0.0 0.0)
  pure (show (map (== kid) hit) ++ show (map (== kid) miss))

scheduled : IO String
scheduled = do
  v <- newWorld {cs = V} 16
  spawnPos v 10
  l <- MkLog <$> newIORef 1 <*> newIORef []
  sch <- newSchedule [counter "a" l, writer l, counter "b" l]
  runFrames sch v l 4
  unwords . reverse <$> readIORef l.lines
  where
    spawnPos : World V -> Int -> IO ()
    spawnPos v k = if k <= 0 then pure () else ignore (spawn v (MkPos 0.0)) >> spawnPos v (k - 1)

    runFrames : Schedule V -> World V -> Log -> Int -> IO ()
    runFrames sch v l k = if k <= 0 then pure () else do
      runSchedule sch (MkTick 0.0 0.0) v
      modifyIORef l.frame (+ 1)
      runFrames sch v l (k - 1)

main : IO ()
main = do
  w <- newWorld {cs = W} cap
  fill w 0
  full <- spawnEmpty w
  cmap w step
  s1 <- total' w
  -- Despawn every odd entity from inside a query over them.
  cfor {q = (Entity, With Odd)} w (\(e, _) => despawn w e)
  n1 <- entityCount w
  odd <- countOf Odd w
  -- Name the even half, then strip velocity from those named.
  cmap {q = (Pos, Without Odd)} {r = Name} w (\(MkPos p, _) => MkName (show p))
  cmap {q = (With Name, Vel)} {r = Without Vel} w (\_ => MkWithout)
  vels <- countOf Vel w
  named <- cfold {q = (Name, Maybe Vel)} w
             (\k, (_, mv) => case mv of Nothing => k + 1; Just _ => k) (the Int 0)
  -- Reuse: a recycled index comes back at a new generation.
  Just e <- spawn w (MkPos 0.0) | Nothing => putStrLn "spawn failed"
  -- A system that despawns its own entity must not have its write
  -- resurrect the component in the dead slot.
  spawnOdd w 1000
  cmapM {q = (Entity, With Odd)} {r = Odd} w (\(x, _) => despawn w x >> pure MkOdd)
  resurrected <- countOf Odd w
  -- A resource is read by every entity that asks.
  g0 <- resource {c = Gravity} w
  setResource w (MkGravity 3.0)
  gsum <- cfold {q = (Pos, Gravity)} w (\k, (_, MkGravity g) => k + g) 0.0
  sched <- scheduled
  pick <- picked
  putStrLn $ unwords
    [ "full", show (isNothing full)
    , "sum", show s1
    , "live", show n1, "odd", show odd
    , "vels", show vels, "named", show named
    , "gen", show (entityGen e)
    , "resurrected", show resurrected
    , "gravity", show g0.grav, show gsum
    , "pick", pick
    , "|", sched ]
