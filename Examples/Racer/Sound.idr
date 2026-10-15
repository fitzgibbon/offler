||| Every sound the racer makes, synthesised as samples: no asset files, the
||| same bytes on every backend. The engine is two seamless loops -- one
||| second of a whole number of cycles each -- crossfaded by speed, since a
||| voice's rate is fixed once it plays.
module Examples.Racer.Sound

import Offler.Math

%default covering

public export
rate : Int
rate = 22050

noise : Int -> Double
noise i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in 2.0 * (x - floor x) - 1.0

||| `seconds` of samples from a function of time and sample index, built
||| with an accumulator: a plain `map` over a range this long is a stack
||| overflow on the JavaScript backend.
synth : Double -> (Double -> Int -> Double) -> List Double
synth seconds f = go (cast (seconds * cast rate) - 1) []
  where
    go : Int -> List Double -> List Double
    go i acc = if i < 0 then acc else go (i - 1) (f (cast i / cast rate) i :: acc)

||| A buzzy engine note at `hz`, which must be a whole number for the loop
||| to join.
export
engine : Double -> List Double
engine hz = synth 1.0 $ \t, _ =>
  let ph = hz * t
      saw = 2.0 * (ph - floor ph) - 1.0
      wob = 1.0 + 0.25 * sin (tau * 5.0 * t)
   in 0.22 * wob * (0.7 * saw + 0.3 * sin (tau * 2.0 * ph))

||| A missile leaving: a falling whistle over a breath of noise.
export
launch : List Double
launch = synth 0.45 $ \t, i =>
  let env = exp (-5.0 * t) * min 1.0 (t / 0.01)
      f = 1400.0 - 1500.0 * t
   in 0.3 * env * (0.6 * sin (tau * f * t) + 0.4 * noise i)

||| A hit: low thump under crackling noise.
export
boom : List Double
boom = synth 0.9 $ \t, i =>
  let env = exp (-4.5 * t) * min 1.0 (t / 0.004)
   in 0.55 * env * (0.6 * noise i * exp (-2.0 * t) + 0.5 * sin (tau * (70.0 - 30.0 * t) * t))

||| A lap completed: two rising tones.
export
chime : List Double
chime = synth 0.6 $ \t, _ =>
  let f = if t < 0.18 then 660.0 else 990.0
      local = if t < 0.18 then t else t - 0.18
      env = exp (-7.0 * local) * min 1.0 (local / 0.005)
   in 0.3 * env * (sin (tau * f * t) + 0.3 * sin (tau * 2.0 * f * t))

||| Metal on tyres, or on another car.
export
scrape : List Double
scrape = synth 0.25 $ \t, i =>
  0.35 * exp (-12.0 * t) * noise i * (0.6 + 0.4 * sin (tau * 180.0 * t))
