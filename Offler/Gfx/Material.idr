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
import public Data.Vect

import Data.Nat

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

  ||| How many texture slots this material declares. `matTextureSlots` and
  ||| `matTextures` are both `Vect` over this, so a material cannot declare
  ||| two slots and then hand back one texture -- which used to compile, and
  ||| silently bound the renderer's white texture to the second slot.
  matTexCount : Nat

  ||| Texture slot names, in binding order. Slot `i` appears to the shaders
  ||| as `t_<name>` (with `s_<name>` its WGSL sampler).
  matTextureSlots : Vect matTexCount String

  ||| The authored WGSL body: `vs` and `fs` entry points written against
  ||| the generated declarations (`VertexIn`, `g`, `o`, `m`, textures,
  ||| `offlerAlpha`).
  matWgsl : String

  ||| The authored GLSL ES 300 stages for the WebGL2 backend, written
  ||| against the same generated declarations (flat block members: `proj`,
  ||| `model`, `lane`, the material's own field names, `t_<name>`).
  matGlslVert : String
  matGlslFrag : String

  ||| Whether this material can draw line-topology meshes: its WGSL provides
  ||| a `vs_line` entry point over `LineIn`, and `matGlslLineVert` a line
  ||| vertex stage. `TopoOk` gates line meshes on this *at compile time* --
  ||| what bevy discovers as a runtime pipeline-specialisation error is a
  ||| missing `So` here. Defaults to False.
  matLineEntry : Bool
  matLineEntry = False

  ||| The GLSL line vertex stage, when `matLineEntry` is True.
  matGlslLineVert : String
  matGlslLineVert = ""

  ||| Whether this material can draw *instanced* batches: its WGSL provides
  ||| a `vs_inst` entry point over `InstIn` (the triangle attributes plus
  ||| the per-instance matrix columns `im0..im3` and `icolor`), and
  ||| `matGlslInstVert` the GLSL stage. `InstOk` gates `drawInstanced` on
  ||| this at compile time. Defaults to False.
  matInstEntry : Bool
  matInstEntry = False

  ||| The GLSL instanced vertex stage, when `matInstEntry` is True.
  matGlslInstVert : String
  matGlslInstVert = ""

  ||| Per-draw: how this value's alpha is honoured.
  alphaMode : m -> AlphaMode

  ||| Per-draw: one entry per declared slot -- exactly `matTexCount` of them,
  ||| which the type now enforces. `Nothing` binds the renderer's built-in
  ||| 1x1 white texture, so an absent map multiplies by one.
  matTextures : m -> Vect matTexCount (Maybe TextureHandle)

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

||| The texture count fits the fixed bindings. A real `LTE` over the
||| interface's own `Nat`, found by proof search -- where this was a `So`
||| over a `Nat`-to-`Int` cast of a list length, which reduced only because
||| every material's slot list happened to be a literal.
public export
0 FitsSlots : (0 m : Type) -> Material m => Type
FitsSlots m = LTE (matTexCount {m}) 4

0 maxTextureSlotsOk : Offler.Gfx.Layout.maxTextureSlots = 4
maxTextureSlotsOk = Refl

||| Whether a material may draw a mesh of the given topology, decided by
||| reduction at the draw site: triangles always; lines only when the
||| material declares its line entry points. This is the type-level
||| counterpart of bevy's per-topology pipeline specialisation.
public export
0 TopoOk : Topology -> (0 m : Type) -> Material m => Type
TopoOk Triangles m = Unit
TopoOk Lines m = So (matLineEntry {m})

||| Whether a material may draw instanced batches: only when it declares
||| its instanced entry points. The same compile-time gate as `TopoOk`.
public export
0 InstOk : (0 m : Type) -> Material m => Type
InstOk m = So (matInstEntry {m})

||| A registered material *type*: the pipelines and shaders for `m` on one
||| renderer, minted by `registerMaterial`. Phantom-typed, so an asset of
||| one material type cannot be created against the pipelines of another --
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

||| A retained material *asset*: bevy's `Handle<M>`. Minted by
||| `addMaterial`, which uploads the material's uniform block into its own
||| slot and records its textures backend-side -- after which drawing with
||| the handle costs no material work per draw at all. The alpha lane data
||| rides in the handle (written into each draw's engine block), so a
||| handle is immutable: `updateMaterial` re-fills the same slot and mints
||| a fresh handle.
export
data Handle : Type -> Type where
  MkHandle : (asset : Int) -> (code, cutoff : Double) -> (blend : Bool) -> Handle m

||| For backends only.
export %inline
handleFor : (asset : Int) -> AlphaMode -> Handle m
handleFor a am = MkHandle a (alphaCode am) (alphaCutoff am) (isBlend am)

export %inline
handleAsset : Handle m -> Int
handleAsset (MkHandle a _ _ _) = a

export %inline
handleCode : Handle m -> Double
handleCode (MkHandle _ c _ _) = c

export %inline
handleCutoff : Handle m -> Double
handleCutoff (MkHandle _ _ c _) = c

export %inline
handleBlend : Handle m -> Bool
handleBlend (MkHandle _ _ _ b) = b

--------------------------------------------------------------------------------
-- What backends derive from an instance

||| The full WGSL source for a material's pipelines.
public export
materialWgsl : Material m => String
materialWgsl =
  wgslMaterialPrologue (matFields {m}) (toList (matTextureSlots {m})) ++ matWgsl {m}

||| The full GLSL stages.
public export
materialGlslVert : Material m => String
materialGlslVert = glslMaterialVertOf Triangles (matFields {m}) (matGlslVert {m})

public export
materialGlslLineVert : Material m => String
materialGlslLineVert = glslMaterialVertOf Lines (matFields {m}) (matGlslLineVert {m})

public export
materialGlslInstVert : Material m => String
materialGlslInstVert = glslMaterialInstVert (matFields {m}) (matGlslInstVert {m})

public export
materialGlslFrag : Material m => String
materialGlslFrag =
  glslMaterialFrag (matFields {m}) (toList (matTextureSlots {m})) (matGlslFrag {m})

||| The bind group layout spec for this material's pipelines.
public export
materialSpec : Material m => String
materialSpec = materialBindSpec (matFields {m}) (cast (matTexCount {m}))

||| Texture slot names as one spec string, for the GL2 backend's sampler
||| binding.
public export
materialTexNames : Material m => String
materialTexNames = joinSemi (toList (matTextureSlots {m}))

||| The material block's byte size.
public export
materialSize : Material m => Int
materialSize = structSize (matFields {m})

||| A draw's texture bindings as four ints, `-1` for empty or white-bound
||| slots -- the fixed arity the C FFI wants. Backends substitute their
||| white texture for `-1` within the declared slot count.
public export
||| Padded to exactly four by `take 4` over the declared slots followed by
||| four blanks, so there is no shorter-than-four case to answer for and no
||| unreachable fallback branch.
texIds : Material m => m -> (Int, Int, Int, Int)
texIds v = unpack (Data.Vect.take 4 padded)
  where
    blanks : Vect 4 Int
    blanks = [-1, -1, -1, -1]

    declared : Vect (matTexCount {m}) Int
    declared = map (maybe (-1) textureIndex) (matTextures v)

    padded : Vect (4 + matTexCount {m}) Int
    padded = rewrite plusCommutative 4 (matTexCount {m}) in declared ++ blanks

    unpack : Vect 4 Int -> (Int, Int, Int, Int)
    unpack [a, b, c, d] = (a, b, c, d)

||| Fill a batch's object slots from `first` -- model matrices against one
||| retained material handle, whose lane data is constant across the batch.
||| Material data is *not* written here: it lives in the asset's own slot,
||| uploaded when the asset was added. The loop is `fillWith`'s --
||| trampolined, bare-comparison bounds.
export
fillModels : (obj : ObjScratch) -> (first : Int)
          -> (code, cutoff : Double) -> List Mat4 -> IO Int
fillModels obj first code cutoff batch =
  fillWith obj first batch $ \model, s =>
    pokeObject obj s model code cutoff 0.0 0.0
