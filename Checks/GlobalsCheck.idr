||| The frame globals are written at hand-written literal offsets since the
||| light loop was unrolled. This reads them back and checks each field
||| landed where `Offler.Gfx.Layout` says it should.
module Main

import Data.Vect
import Offler.Camera
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Uniform
import Offler.Light
import Offler.Math
import Offler.Transform

at : GlobalScratch -> Int -> IO Double
at a i = case window {w = 1} a i of
           Just o => peek a o
           Nothing => pure (0.0/0.0)

main : IO ()
main = do
  a <- newGlobalScratch
  let ls = Offler.Light.lights 0.25
             [ MkDirectional (v3 0.0 (-1.0) 0.0) (rgb 0.1 0.2 0.3)
             , MkDirectional (v3 1.0 0.0 0.0) (rgb 0.4 0.5 0.6) ]
  pokeGlobals a (perspectiveCamera (at (v3 1.0 2.0 3.0))) 1.5 ls 7.0 False
  -- cam at 32..34, time at 35
  eye <- traverse (at a) (the (List Int) [32, 33, 34, 35])
  putStrLn ("cam+time " ++ show eye)
  -- counts at 36: light count, ambient
  cnt <- traverse (at a) (the (List Int) [36, 37])
  putStrLn ("counts   " ++ show cnt)
  -- lightDirs at 40: first two normalised, last two zeroed
  ds <- traverse (at a) (the (List Int) [40, 41, 42, 44, 45, 46, 48, 52])
  putStrLn ("dirs     " ++ show ds)
  -- lightColors at 56
  cs <- traverse (at a) (the (List Int) [56, 57, 58, 60, 61, 62, 64, 68])
  putStrLn ("colors   " ++ show cs)
