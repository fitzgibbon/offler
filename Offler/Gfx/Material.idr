||| The material abstraction: offler's rendition of bevy's `Material` trait.
|||
||| A material is a *type* describing a uniform block, texture slots and
||| shaders, plus *values* carrying the per-draw data. The correspondence,
||| member for member:
|||
|||   bevy `trait Material: AsBindGroup`   this interface
|||   `#[derive(AsBindGroup)]` uniforms    `matFields` + `writeMat`
|||   `#[texture(n)] #[sampler(n)]`        `matTextureSlots` + `matTextures`
|||   `fragment_shader()` etc.             `matWgsl`, `matGlslVert`, `matGlslFrag`
|||   `alpha_mode()`                       `alphaMode`
|||   `MaterialPlugin::<M>` registration   `registerMaterial` -> `MaterialId m`
|||   `Handle<Image>`                      `TextureHandle`
|||
||| Where bevy derives the GPU-facing description with a proc macro, offler
||| derives it in the type checker: the struct text, the bind group spec and
||| the shader prologue are generated from `matFields`, and
||| `registerMaterial` demands a compile-time proof that the block fits its
||| 256-byte slot and the texture count fits the fixed bindings.
module Offler.Gfx.Material

import public Data.So

-- Public: the registration bounds (`FitsSlot`, `FitsSlots`) expand to
-- expressions over `structSize`, `objStride` and friends, which client
-- modules must therefore be able to name and *reduce* without importing the
-- layout themselves. `MatWriter` appears in `writeMat`'s type likewise.
import public Offler.Gfx.Layout
import public Offler.Gfx.Uniform

import Offler.Color
import Offler.Gfx.Array
import Offler.Math

%default total

--------------------------------------------------------------------------------
-- Alpha

||| How a material's alpha channel is honoured -- bevy's `AlphaMode`,
||| reduced to the three modes one forward pass supports.
public export
data AlphaMode : Type where
  ||| Alpha is ignored; drawn in the opaque pass, depth-written.
  Opaque : AlphaMode
  ||| Fragments below the cutoff are discarded (the shader calls the
  ||| generated `offlerAlpha` helper); otherwise as `Opaque`. Cutout
  ||| foliage, grilles.
  Mask : (cutoff : Double) -> AlphaMode
  ||| Alpha-blended, depth-tested but not written, drawn after every opaque
  ||| draw in a back-to-front sorted phase.
  Blend : AlphaMode

||| The lane encoding the shaders read: 0 opaque, 1 mask, 2 blend.
public export
alphaCode : AlphaMode -> Double
alphaCode Opaque = 0.0
alphaCode (Mask _) = 1.0
alphaCode Blend = 2.0

public export
alphaCutoff : AlphaMode -> Double
alphaCutoff (Mask c) = c
alphaCutoff _ = 0.0

public export
isBlend : AlphaMode -> Bool
isBlend Blend = True
isBlend _ = False

--------------------------------------------------------------------------------
-- Textures

||| A texture the renderer has accepted, usable only with the renderer that
||| minted it: bevy's `Handle<Image>`. The constructor is private;
||| `textureHandle`/`textureIndex` exist for backend modules.
export
data TextureHandle : Type where
  MkTextureHandle : Int -> TextureHandle

export %inline
textureHandle : Int -> TextureHandle
textureHandle = MkTextureHandle

export %inline
textureIndex : TextureHandle -> Int
textureIndex (MkTextureHandle i) = i

||| Where texture bytes come from. `FromPath` is a URL in the browser and a
||| file path natively; `FromBase64` is decoded in place (a data URL in the
||| browser, stb_image natively) and is what lets a page opened straight off
||| disk carry its own images -- `file://` origins taint canvas image loads.
||| Common formats decode on every backend: PNG, JPEG, GIF and BMP at least.
public export
data TextureSource : Type where
  FromPath : String -> TextureSource
  FromBase64 : (mime : String) -> (base64 : String) -> TextureSource

--------------------------------------------------------------------------------
-- The interface

||| A material type. The first five members depend only on the type -- call
||| them with an explicit `{m = MyMaterial}` -- and describe the GPU-facing
||| contract; the last three read a value's per-draw data.
public export
interface Material m where
  ||| The uniform block, in `Offler.Gfx.Layout` field terms. The struct
  ||| text, its std140/WGSL offsets and the bind-group entry are generated
  ||| from this; `writeMat` must fill it lane for lane.
  matFields : List Field

  ||| Texture slot names, in binding order. Slot `i` appears to the shaders
  ||| as `t_<name>` (with `s_<name>` its WGSL sampler).
  matTextureSlots : List String

  ||| The authored WGSL body: `vs` and `fs` entry points written against
  ||| the generated declarations (`VertexIn`, `g`, `o`, `m`, textures,
  ||| `offlerAlpha`).
  matWgsl : String

  ||| The authored GLSL ES 300 stages for the WebGL2 backend, written
  ||| against the same generated declarations (flat block members: `proj`,
  ||| `model`, `lane`, the material's own field names, `t_<name>`).
  matGlslVert : String
  matGlslFrag : String

  ||| Per-draw: how this value's alpha is honoured.
  alphaMode : m -> AlphaMode

  ||| Per-draw: one entry per declared slot. `Nothing` binds the renderer's
  ||| built-in 1x1 white texture, so an absent map multiplies by one.
  matTextures : m -> List (Maybe TextureHandle)

  ||| Per-draw: fill the uniform block, lane by 16-byte lane, matching
  ||| `matFields`.
  writeMat : MatWriter -> m -> IO ()

||| What `registerMaterial` demands `So` proofs of, computed from the type
||| at the call site: `FitsSlot m` -- the uniform block fits its 256-byte
||| slot -- and `FitsSlots m` -- the texture count fits the fixed bindings.
||| These are type synonyms rather than a wrapped `Bool` predicate: the
||| compile-time evaluator reduces the inline expressions readily, where the
||| same conditions behind a function application (or under `(&&)`'s
||| laziness) got stuck -- and a proof that cannot reduce is a proof nobody
||| can give.
public export
0 FitsSlot : (0 m : Type) -> Material m => Type
FitsSlot m = So (structSize (matFields {m}) <= objStride)

public export
0 FitsSlots : (0 m : Type) -> Material m => Type
FitsSlots m = So (the Int (cast (length (matTextureSlots {m}))) <= maxTextureSlots)

||| A registered material: the pipelines and shaders for `m` on one
||| renderer, minted by `registerMaterial`. Phantom-typed, so a draw cannot
||| pair a value of one material type with the pipelines of another --
||| that is a type error, not a wrong image.
export
data MaterialId : Type -> Type where
  MkMaterialId : Int -> MaterialId m

export %inline
materialId : Int -> MaterialId m
materialId = MkMaterialId

export %inline
materialIdIndex : MaterialId m -> Int
materialIdIndex (MkMaterialId i) = i

--------------------------------------------------------------------------------
-- What backends derive from an instance

||| The full WGSL source for a material's pipelines.
public export
materialWgsl : Material m => String
materialWgsl =
  wgslMaterialPrologue (matFields {m}) (matTextureSlots {m}) ++ matWgsl {m}

||| The full GLSL pair.
public export
materialGlslVert : Material m => String
materialGlslVert = glslMaterialVert (matFields {m}) (matGlslVert {m})

public export
materialGlslFrag : Material m => String
materialGlslFrag =
  glslMaterialFrag (matFields {m}) (matTextureSlots {m}) (matGlslFrag {m})

||| The bind group layout spec for this material's pipelines.
public export
materialSpec : Material m => String
materialSpec =
  materialBindSpec (matFields {m}) (cast (length (matTextureSlots {m})))

||| Texture slot names as one spec string, for the GL2 backend's sampler
||| binding.
public export
materialTexNames : Material m => String
materialTexNames = joinSemi (matTextureSlots {m})

||| The material block's byte size.
public export
materialSize : Material m => Int
materialSize = structSize (matFields {m})

||| A draw's texture bindings as four ints, `-1` for empty or white-bound
||| slots -- the fixed arity the C FFI wants. Backends substitute their
||| white texture for `-1` within the declared slot count.
public export
texIds : Material m => m -> (Int, Int, Int, Int)
texIds v =
  case map (maybe (-1) textureIndex) (matTextures v) ++ [-1, -1, -1, -1] of
    (a :: b :: c :: d :: _) => (a, b, c, d)
    _ => (-1, -1, -1, -1)

||| Fill a batch's object and material slots from `first`, one slot index
||| for both buffers per item, returning the next free index. The loop is
||| `fillWith`'s -- trampolined, bare-comparison bounds -- with this
||| material's writer and alpha lane per item.
export
fillBatch : Material m => (obj : ObjScratch) -> (mat : ObjScratch)
         -> (first : Int) -> List (Mat4, m) -> IO Int
fillBatch obj mat first batch =
  fillWith obj first batch $ \p, s => case p of
    (model, v) => do
      let am = alphaMode v
      pokeObject obj s model (alphaCode am) (alphaCutoff am) 0.0 0.0
      writeMat (matWriter mat s) v
