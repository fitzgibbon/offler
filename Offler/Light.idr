||| The lights the standard shader evaluates: up to `maxLights` directional
||| lights plus a flat ambient term. Bevy's `DirectionalLight` and
||| `AmbientLight`, reduced to what a single forward pass without shadows
||| can honour.
module Offler.Light

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

public export
record Lights where
  constructor MkLights
  ||| Flat term added to every lit surface, in [0, 1]-ish.
  ambient : Double
  ||| The first `maxLights` (4) are uploaded; the rest are ignored.
  directionals : List DirectionalLight

||| Overhead sun, slightly warm, gentle ambient.
public export
defaultLights : Lights
defaultLights =
  MkLights 0.08 [MkDirectional (MkV3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.98 0.92)]

||| A second, cooler fill light from the opposite side, for scenes that
||| want their shadow sides readable.
public export
twoLights : Lights
twoLights =
  MkLights 0.06
    [ MkDirectional (MkV3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.96 0.88)
    , MkDirectional (MkV3 0.6 (-0.3) 0.5) (dim 0.35 (rgb 0.55 0.65 1.0))
    ]
