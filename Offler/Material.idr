||| The standard material: bevy's `StandardMaterial`, as the bundled
||| implementation of the `Offler.Gfx.Material` interface. Anything it does
||| -- its uniform block, its texture slot, its shaders -- a user material
||| does the same way; nothing here is engine-privileged.
module Offler.Material

import Offler.Color
import Offler.Gfx.Layout
import Offler.Gfx.Material
import Offler.Gfx.Uniform
import Offler.Shaders

%default total

||| A procedural surface texture, evaluated in the shader over the
||| *object-space* position so it stays welded to the surface as the object
||| moves and spins. The scale is cells (or noise features) per unit of the
||| unscaled mesh. Composes with `baseColorTexture`: the sample and the
||| pattern both modulate the base colour.
public export
data Pattern : Type where
  Plain : Pattern
  Checker : (scale : Double) -> Pattern
  ||| Value-noise fbm, modulating the base colour's brightness.
  Noise : (scale : Double) -> Pattern

||| The shaders take the pattern as two floats in the material's `params`
||| lane, since a uniform cannot be a sum type.
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
record StandardMaterial where
  constructor MkStandardMaterial
  ||| Linear RGB plus alpha, multiplied by the texture sample. How the alpha
  ||| is honoured is `alpha`'s business.
  baseColor : Color
  ||| Multiplied into `baseColor` texel by texel -- bevy's
  ||| `base_color_texture`. `Nothing` binds the renderer's 1x1 white.
  baseColorTexture : Maybe TextureHandle
  ||| Added to the lit result, unattenuated. `dim` scales intensity.
  emissive : Color
  ||| 0 dielectric to 1 metal: scales the specular tint towards the base
  ||| colour and dims the diffuse term.
  metallic : Double
  ||| Perceptual roughness, 0 mirror to 1 matte.
  roughness : Double
  ||| Skip lighting entirely and emit the (textured, patterned) base colour.
  unlit : Bool
  ||| Procedural modulation of the base colour.
  pattern : Pattern
  ||| Opaque, mask with a cutoff, or blended -- bevy's `alpha_mode`.
  alpha : AlphaMode

||| A lit, matte, non-metallic, opaque surface --
||| `StandardMaterial::default()` with the given base colour.
public export
lit : Color -> StandardMaterial
lit c = MkStandardMaterial c Nothing black 0.0 0.5 False Plain Opaque

||| The base colour exactly, no lighting. What 2D wants.
public export
unlit : Color -> StandardMaterial
unlit c = MkStandardMaterial c Nothing black 0.0 0.5 True Plain Opaque

||| A light source's own surface: black base so only the glow shows.
public export
glowing : Color -> StandardMaterial
glowing c = MkStandardMaterial black Nothing c 0.0 0.5 False Plain Opaque

public export
withMetallic : Double -> StandardMaterial -> StandardMaterial
withMetallic m mat = { metallic := m } mat

public export
withRoughness : Double -> StandardMaterial -> StandardMaterial
withRoughness r mat = { roughness := r } mat

public export
withEmissive : Color -> StandardMaterial -> StandardMaterial
withEmissive e mat = { emissive := e } mat

public export
withPattern : Pattern -> StandardMaterial -> StandardMaterial
withPattern p mat = { pattern := p } mat

public export
withTexture : TextureHandle -> StandardMaterial -> StandardMaterial
withTexture t mat = { baseColorTexture := Just t } mat

public export
withAlpha : AlphaMode -> StandardMaterial -> StandardMaterial
withAlpha a mat = { alpha := a } mat

||| The uniform block: three lanes. Lane 0 `baseColor`; lane 1 `emissive`
||| rgb with the unlit flag in `w`; lane 2 `params` = metallic, roughness,
||| pattern code, pattern scale.
|||
||| `public export`, as every `Material` implementation must be: the
||| `registerMaterial` size bound is proved by *reducing* `matFields` at the
||| registration site, which needs the method bodies visible there.
public export
Material StandardMaterial where
  matFields =
    [ MkField "baseColor" Vec4
    , MkField "emissive" Vec4
    , MkField "params" Vec4
    ]
  matTextureSlots = ["base_color"]
  matWgsl = wgslSrc
  matGlslVert = vertSrc
  matGlslFrag = fragSrc
  alphaMode v = v.alpha
  matTextures v = [v.baseColorTexture]
  writeMat w v = do
    putColor w 0 v.baseColor
    putVec4 w 1 v.emissive.red v.emissive.green v.emissive.blue
              (if v.unlit then 1.0 else 0.0)
    putVec4 w 2 v.metallic v.roughness
              (patternCode v.pattern) (patternScale v.pattern)
