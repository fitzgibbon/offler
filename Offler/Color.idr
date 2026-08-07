||| Colours, in linear RGB -- what the shaders blend in. The fragment shaders
||| apply the gamma encode at the end, so everything CPU-side stays linear,
||| which is bevy's `LinearRgba` convention.
module Offler.Color

%default total

public export
record Color where
  constructor MkColor
  red, green, blue, alpha : Double

public export
rgb : Double -> Double -> Double -> Color
rgb r g b = MkColor r g b 1.0

public export
rgba : Double -> Double -> Double -> Double -> Color
rgba = MkColor

||| One sRGB channel to linear.
public export
toLinear : Double -> Double
toLinear x = if x <= 0.04045 then x / 12.92 else pow ((x + 0.055) / 1.055) 2.4

||| A colour authored in familiar sRGB terms -- what a picker or a CSS value
||| gives -- converted to the linear values the shaders blend in. Without
||| this, the gamma encode at the end of the shader washes everything toward
||| pastel. Bevy's `Color::srgb`.
public export
srgb : Double -> Double -> Double -> Color
srgb r g b = MkColor (toLinear r) (toLinear g) (toLinear b) 1.0

public export
srgba : Double -> Double -> Double -> Double -> Color
srgba r g b a = MkColor (toLinear r) (toLinear g) (toLinear b) a

public export
withAlpha : Double -> Color -> Color
withAlpha a (MkColor r g b _) = MkColor r g b a

||| Scale the colour channels, leaving alpha alone. What an emissive intensity
||| multiplies by.
public export
dim : Double -> Color -> Color
dim k (MkColor r g b a) = MkColor (k * r) (k * g) (k * b) a

||| Hue in [0, 1) around the wheel, saturation and lightness in [0, 1].
||| Authored in sRGB terms, like `srgb`, and converted to linear.
public export
hsl : (h, s, l : Double) -> Color
hsl h s l =
  let h' = (h - floor h) * 6.0
      c = (1.0 - abs (2.0 * l - 1.0)) * s
      x = c * (1.0 - abs (h' - 2.0 * floor (h' / 2.0) - 1.0))
      m = l - c / 2.0
      (r, g, b) = if h' < 1.0 then (c, x, 0.0)
                  else if h' < 2.0 then (x, c, 0.0)
                  else if h' < 3.0 then (0.0, c, x)
                  else if h' < 4.0 then (0.0, x, c)
                  else if h' < 5.0 then (x, 0.0, c)
                  else (c, 0.0, x)
   in srgb (r + m) (g + m) (b + m)

public export
white : Color
white = rgb 1.0 1.0 1.0

public export
black : Color
black = rgb 0.0 0.0 0.0
