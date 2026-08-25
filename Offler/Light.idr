||| The lights the standard shader evaluates: up to `maxLights` directional
||| lights plus a flat ambient term. Bevy's `DirectionalLight` and
||| `AmbientLight`, reduced to what a single forward pass without shadows
||| can honour.
module Offler.Light

import Data.Vect

import Offler.Color
import Offler.Gfx.Layout
import Offler.Math

%default total

public export
record DirectionalLight where
  constructor MkDirectional
  ||| The direction light *travels*, world space. Normalised by the
  ||| renderer before upload, so callers may hand in any non-zero vector.
  direction : V3
  ||| Linear RGB; alpha ignored.
  color : Color

||| The four directional slots the globals block carries. `Vect 4 (Maybe
||| DirectionalLight)` rather than an unbounded list: "the first four are
||| uploaded, the rest are ignored" was a docstring the caller could not see
||| and the renderer enforced with a `take`. Now the ceiling is the type, and
||| an empty slot is visibly `Nothing` rather than silently dropped.
public export
LightSlots : Type
LightSlots = Vect 4 (Maybe DirectionalLight)

0 lightSlotsOk : 4 = Offler.Gfx.Layout.maxLights
lightSlotsOk = Refl

public export
record Lights where
  constructor MkLights
  ||| Flat term added to every lit surface, in [0, 1]-ish.
  ambient : Double
  ||| Up to `maxLights` directional lights, one per slot.
  directionals : LightSlots

||| `Lights` from however many directionals you have, keeping the first four.
public export
lights : Double -> List DirectionalLight -> Lights
lights amb ds = MkLights amb (go ds)
  where
    go : List DirectionalLight -> LightSlots
    go [] = [Nothing, Nothing, Nothing, Nothing]
    go [a] = [Just a, Nothing, Nothing, Nothing]
    go [a, b] = [Just a, Just b, Nothing, Nothing]
    go [a, b, c] = [Just a, Just b, Just c, Nothing]
    go (a :: b :: c :: d :: _) = [Just a, Just b, Just c, Just d]

||| How many slots are filled -- what the shader's light count reads.
public export
lightCount : LightSlots -> Double
lightCount = foldl step 0.0
  where
    step : Double -> Maybe DirectionalLight -> Double
    step n Nothing = n
    step n (Just _) = n + 1.0

||| Overhead sun, slightly warm, gentle ambient.
public export
defaultLights : Lights
defaultLights =
  lights 0.08 [MkDirectional (MkV3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.98 0.92)]

||| A second, cooler fill light from the opposite side, for scenes that
||| want their shadow sides readable.
public export
twoLights : Lights
twoLights =
  lights 0.06
    [ MkDirectional (MkV3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.96 0.88)
    , MkDirectional (MkV3 0.6 (-0.3) 0.5) (dim 0.35 (rgb 0.55 0.65 1.0))
    ]
