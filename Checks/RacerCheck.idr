||| The racer's race, run headless: eight CPU cars, three simulated minutes
||| at thirty steps a second, through the same systems the game schedules
||| -- start lights, AI, physics, collisions, lap timing, missiles. Every
||| car must keep lapping (none stuck on a wall, on the ramp or spun into
||| the grass for good), every lap time must be plausible, nothing may go
||| NaN, and the jump must throw cars higher than its lip.
module Checks.RacerCheck

import Data.IORef
import Data.List
import Data.String
import Examples.Racer.Scene
import Examples.Racer.Track
import Offler.Ecs

%default covering


steps : World W -> Schedule W -> Int -> Double -> IO ()
steps w sch n t =
  if n <= 0 then pure () else do
    runSchedule sch (MkTick t (1.0 / 30.0)) w
    steps w sch (n - 1) (t + 1.0 / 30.0)

spawnAll : World W -> Int -> IO ()
spawnAll w k = if k >= 8 then pure () else ignore (spawnCar w k False) >> spawnAll w (k + 1)

main : IO ()
main = do
  w <- newRaceWorld
  spawnAll w 0
  fired <- newIORef (the Int 0)
  hits <- newIORef (the Int 0)
  peak <- newIORef 0.0
  sch <- newSchedule (simulation ++ [tallyFights fired hits, \_, w => highest w >>= \h => modifyIORef peak (max h)])
  -- A browser's first frame takes no time.
  runSchedule sch (MkTick 0.0 0.0) w
  steps w sch (180 * 30) 0.0
  rs <- results w
  nf <- readIORef fired
  nh <- readIORef hits
  top <- readIORef peak
  let laps = map (\(_, l, _) => l) rs
      bests = map (\(_, _, b) => b) rs
      minLaps = foldl min 1000 laps
      sane = all (\b => b == b && b > 10.0 && b < 60.0) bests
      fights = nf > 0 && nh > 0
      flew = top > rampTop && top < 10.0
  putStrLn $ (if minLaps >= 3 && sane && fights && flew then "race ok" else "race FAIL")
    ++ ": laps " ++ unwords (map show laps)
    ++ " best " ++ unwords (map (\b => show (the Int (cast (b * 10.0)))) bests)
    ++ " missiles " ++ show nf ++ " hits " ++ show nh
    ++ " peak " ++ show (the Int (cast (top * 10.0)))
