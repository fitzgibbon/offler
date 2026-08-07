||| What a surface is made of: bevy's `StandardMaterial`, reduced to the
||| fields one forward-pass shader honours.
module Offler.Material

import Offler.Color

%default total

||| A procedural surface texture, evaluated in the shader over the
||| *object-space* position so it stays welded to the surface as the object
||| moves and spins. The scale is cells (or noise features) per world unit
||| of the unscaled mesh.
public export
data Pattern : Type where
  Plain : Pattern
  Checker : (scale : Double) -> Pattern
  ||| Value-noise fbm, modulating the base colour's brightness.
  Noise : (scale : Double) -> Pattern

||| The shaders take the pattern as two floats in the object uniform's spare
||| `params` lanes, since a uniform cannot be a sum type.
public export
patternCode : Pattern -> Double
patternCode Plain = 0.0
patternCode (Checker _) = 1.0
patternCode (Noise _) = 2.0

public export
patternScale : Pattern -> Double
patternScale Plain = 0.0
patternScale (Checker s) = s
patternScale (Noise s) = s

public export
record Material where
  constructor MkMaterial
  ||| Linear RGB plus alpha. Alpha is honoured by the blended line pipeline;
  ||| the mesh pipeline is opaque, as bevy's default alpha mode is.
  baseColor : Color
  ||| Added to the lit result, unattenuated. `dim` scales intensity.
  emissive : Color
  ||| 0 dielectric to 1 metal: scales the specular tint towards the base
  ||| colour and dims the diffuse term.
  metallic : Double
  ||| Perceptual roughness, 0 mirror to 1 matte.
  roughness : Double
  ||| Skip lighting entirely and emit `baseColor` as-is.
  unlit : Bool
  ||| Procedural modulation of the base colour -- bevy's
  ||| `base_color_texture`, without the image.
  pattern : Pattern

||| A lit, matte, non-metallic surface -- bevy's `StandardMaterial::default()`
||| with the given base colour.
public export
lit : Color -> Material
lit c = MkMaterial c black 0.0 0.5 False Plain

||| The base colour exactly, no lighting. What lines and 2D want.
public export
unlit : Color -> Material
unlit c = MkMaterial c black 0.0 0.5 True Plain

||| A light source's own surface: black base so only the glow shows.
public export
glowing : Color -> Material
glowing c = MkMaterial black c 0.0 0.5 False Plain

public export
withMetallic : Double -> Material -> Material
withMetallic m mat = { metallic := m } mat

public export
withRoughness : Double -> Material -> Material
withRoughness r mat = { roughness := r } mat

public export
withEmissive : Color -> Material -> Material
withEmissive e mat = { emissive := e } mat

public export
withPattern : Pattern -> Material -> Material
withPattern p mat = { pattern := p } mat

||| The shading mode the shader receives, since a uniform cannot be a sum
||| type: 0 lit, 1 unlit.
public export
modeCode : Material -> Double
modeCode m = if m.unlit then 1.0 else 0.0
