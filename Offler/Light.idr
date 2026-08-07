||| The lights the standard shader evaluates: one directional light plus a
||| flat ambient term. Bevy's `DirectionalLight` and `AmbientLight`, reduced
||| to what a single forward pass without shadows can honour.
module Offler.Light

import Offler.Color
import Offler.Math

%default total

public export
record Lights where
  constructor MkLights
  ||| The direction light *travels*, world space. Normalised by the renderer
  ||| before upload, so callers may hand in any non-zero vector.
  direction : V3
  ||| Linear RGB; alpha ignored.
  color : Color
  ||| Flat term added to every lit surface, in [0, 1]-ish.
  ambient : Double

||| Overhead sun, slightly warm, gentle ambient.
public export
defaultLights : Lights
defaultLights = MkLights (MkV3 (-0.4) (-1.0) (-0.3)) (rgb 1.0 0.98 0.92) 0.08
