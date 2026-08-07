||| One description of everything the CPU and the GPU have to agree about.
|||
||| These contracts would otherwise be asserted independently in up to four
||| places: a `struct` and a `@group/@binding` line in the WGSL, a
||| `layout(std140)` block in the GLSL, a `#define` in the C shim, and a
||| hand-counted constant in each Idris backend. Nothing would check them
||| against each other, and disagreement shows up either as a validation error
||| at pipeline creation or -- worse -- as silently misread fields.
|||
||| Here the description is the source. The shader prologues are generated
||| from it, the backends are handed the layout as spec strings they parse
||| once per pipeline, and the Idris offsets are **literals proved equal to
||| the computed layout**. Material types bring their own field lists (the
||| bevy `AsBindGroup` role); the same generators run over those, so a custom
||| material's struct, bindings and size bound are derived, not asserted.
|||
||| The uniform layout rules below are WGSL's; for the field types offler
||| admits (f32, vec2, vec3, vec4, mat4 -- no arrays, no nested structs)
||| GLSL's std140 lays out identically, which is what lets the WebGL2 backend
||| share the same scratch buffers byte for byte.
module Offler.Gfx.Layout

import Data.String

%default total

--------------------------------------------------------------------------------
-- Fields

||| The subset of WGSL types offler uses. Size and alignment follow the
||| uniform address space rules, which is where the padding comes from.
public export
data FieldTy = F32 | Vec2 | Vec3 | Vec4 | Mat4

public export
sizeOf : FieldTy -> Int
sizeOf F32 = 4
sizeOf Vec2 = 8
sizeOf Vec3 = 12
sizeOf Vec4 = 16
sizeOf Mat4 = 64

public export
alignOf : FieldTy -> Int
alignOf F32 = 4
alignOf Vec2 = 8
alignOf Vec3 = 16
alignOf Vec4 = 16
alignOf Mat4 = 16

public export
wgslTy : FieldTy -> String
wgslTy F32 = "f32"
wgslTy Vec2 = "vec2<f32>"
wgslTy Vec3 = "vec3<f32>"
wgslTy Vec4 = "vec4<f32>"
wgslTy Mat4 = "mat4x4<f32>"

public export
glslTy : FieldTy -> String
glslTy F32 = "float"
glslTy Vec2 = "vec2"
glslTy Vec3 = "vec3"
glslTy Vec4 = "vec4"
glslTy Mat4 = "mat4"

||| Float32 components. A vertex attribute may have at most four, which is
||| what `attrsOk` below checks.
public export
components : FieldTy -> Int
components F32 = 1
components Vec2 = 2
components Vec3 = 3
components Vec4 = 4
components Mat4 = 16

public export
record Field where
  constructor MkField
  name : String
  ty : FieldTy

public export
roundUp : Int -> Int -> Int
roundUp n a = ((n + a - 1) `div` a) * a

||| Byte offset just past the given fields in the uniform address space, laid
||| out from `at`.
public export
endAt : Int -> List Field -> Int
endAt at [] = at
endAt at (MkField _ t :: fs) = endAt (roundUp at (alignOf t) + sizeOf t) fs

||| A struct is rounded up to the largest member alignment, which is 16 for
||| everything here.
public export
structSize : List Field -> Int
structSize fs = roundUp (endAt 0 fs) 16

||| Vertex attributes pack tightly -- the 16-byte rules belong to the uniform
||| address space alone, so `endAt` is the wrong accumulator for them.
public export
packedEnd : Int -> List Field -> Int
packedEnd at [] = at
packedEnd at (MkField _ t :: fs) = packedEnd (at + sizeOf t) fs

public export
joinSemi : List String -> String
joinSemi [] = ""
joinSemi [x] = x
joinSemi (x :: xs) = x ++ ";" ++ joinSemi xs

--------------------------------------------------------------------------------
-- What the engine itself binds

||| The frame globals, binding 0: written once per frame by the engine.
public export
globalFields : List Field
globalFields =
  [ MkField "proj" Mat4
  , MkField "view" Mat4
  , MkField "cam" Vec3
  , MkField "time" F32
  , MkField "lightDir" Vec3
  , MkField "ambient" F32
  , MkField "lightColor" Vec3
  , MkField "pad0" F32
  ]

||| The per-draw engine block, binding 1, picked out by a dynamic offset:
||| the model matrix, and the lane the alpha mode rides in --
||| `lane = (alphaMode, cutoff, unused, unused)`. Material data does *not*
||| live here: it has its own block, described by the material type.
public export
objFields : List Field
objFields =
  [ MkField "model" Mat4
  , MkField "lane" Vec4
  ]

||| The mesh vertex: position, normal, texture coordinates, packed tightly.
public export
meshVertexFields : List Field
meshVertexFields = [MkField "pos" Vec3, MkField "normal" Vec3, MkField "uv" Vec2]

||| The line vertex: a position padded out to 16 bytes so `poke16` fills four
||| vertices per foreign call. Lines have their own pipeline and shaders, so
||| no other attribute needs faking.
public export
lineVertexFields : List Field
lineVertexFields = [MkField "pos" Vec3]

||| No vertex attribute may need more than one location.
public export
attrsOk : List Field -> Bool
attrsOk [] = True
attrsOk (MkField _ t :: fs) = components t <= 4 && attrsOk fs

0 meshVertexFieldsOk : Offler.Gfx.Layout.attrsOk Offler.Gfx.Layout.meshVertexFields = True
meshVertexFieldsOk = Refl

0 lineVertexFieldsOk : Offler.Gfx.Layout.attrsOk Offler.Gfx.Layout.lineVertexFields = True
lineVertexFieldsOk = Refl

--------------------------------------------------------------------------------
-- Generated WGSL

public export
wgslStruct : String -> List Field -> String
wgslStruct nm fs = "struct " ++ nm ++ " {\n" ++ concatMap line fs ++ "};\n"
  where
    line : Field -> String
    line (MkField n t) = "  " ++ n ++ " : " ++ wgslTy t ++ ",\n"

public export
wgslVertexStruct : String -> List Field -> String
wgslVertexStruct nm fs = "struct " ++ nm ++ " {\n" ++ go 0 fs ++ "};\n"
  where
    go : Int -> List Field -> String
    go _ [] = ""
    go i (MkField n t :: fs) =
      "  @location(" ++ show i ++ ") " ++ n ++ " : " ++ wgslTy t ++ ",\n"
        ++ go (i + 1) fs

||| The declarations every pipeline shares: the two vertex structs, the
||| globals bound as `g`, and the per-draw engine block bound as `o`.
public export
wgslEngineDecls : String
wgslEngineDecls =
  wgslVertexStruct "VertexIn" meshVertexFields
    ++ wgslVertexStruct "LineIn" lineVertexFields
    ++ wgslStruct "Globals" globalFields
    ++ "@group(0) @binding(0) var<uniform> g : Globals;\n"
    ++ wgslStruct "Obj" objFields
    ++ "@group(0) @binding(1) var<uniform> o : Obj;\n"

||| The alpha-mask helper, generated so every material's fragment shader can
||| honour `Mask` by calling it on its final colour. `o.lane.x` is the alpha
||| mode (0 opaque, 1 mask, 2 blend) and `o.lane.y` the cutoff.
public export
wgslAlphaHelper : String
wgslAlphaHelper =
  "fn offlerAlpha(c : vec4<f32>) -> vec4<f32> {\n"
    ++ "  if (o.lane.x > 0.5 && o.lane.x < 1.5 && c.a < o.lane.y) { discard; }\n"
    ++ "  return c;\n}\n"

||| Texture and sampler declarations for a material's slots: slot `i` binds
||| `t_<name>` at binding `3 + 2i` and `s_<name>` at `4 + 2i`.
public export
wgslTextureDecls : List String -> String
wgslTextureDecls names = go 0 names
  where
    go : Int -> List String -> String
    go _ [] = ""
    go i (n :: ns) =
      "@group(0) @binding(" ++ show (3 + 2 * i) ++ ") var t_" ++ n ++ " : texture_2d<f32>;\n"
        ++ "@group(0) @binding(" ++ show (4 + 2 * i) ++ ") var s_" ++ n ++ " : sampler;\n"
        ++ go (i + 1) ns

||| Everything prepended to a material's authored WGSL: the engine
||| declarations, the material's own uniform struct bound as `m`, its
||| textures, and the alpha helper. The authored body supplies `vs` and `fs`
||| entry points against these names.
public export
wgslMaterialPrologue : (fields : List Field) -> (textures : List String) -> String
wgslMaterialPrologue fields textures =
  wgslEngineDecls
    ++ wgslStruct "Mat" fields
    ++ "@group(0) @binding(2) var<uniform> m : Mat;\n"
    ++ wgslTextureDecls textures
    ++ wgslAlphaHelper

||| The line pipeline's prologue: engine declarations only.
public export
wgslLinePrologue : String
wgslLinePrologue = wgslEngineDecls

--------------------------------------------------------------------------------
-- Generated GLSL

||| Members of a std140 block, flat in the shader's namespace -- `model`,
||| `lane`, `proj` and the material's field names are therefore reserved
||| words for authored GLSL bodies. Blocks are bound to their binding points
||| by name at link time, so no layout qualifier is needed here.
glslBlock : String -> List Field -> String
glslBlock nm fs =
  "layout(std140) uniform " ++ nm ++ " {\n" ++ concatMap line fs ++ "};\n"
  where
    line : Field -> String
    line (MkField n t) = "  " ++ glslTy t ++ " " ++ n ++ ";\n"

public export
glslVertexIn : String
glslVertexIn = go 0 meshVertexFields
  where
    go : Int -> List Field -> String
    go _ [] = ""
    go i (MkField n t :: fs) =
      "layout(location=" ++ show i ++ ") in " ++ glslTy t ++ " " ++ n ++ ";\n"
        ++ go (i + 1) fs

public export
glslLineVertexIn : String
glslLineVertexIn = "layout(location=0) in vec3 pos;\n"

public export
glslEngineBlocks : String
glslEngineBlocks = glslBlock "Globals" globalFields ++ glslBlock "Obj" objFields

public export
glslAlphaHelper : String
glslAlphaHelper =
  "vec4 offlerAlpha(vec4 c) {\n"
    ++ "  if (lane.x > 0.5 && lane.x < 1.5 && c.a < lane.y) discard;\n"
    ++ "  return c;\n}\n"

||| `uniform sampler2D t_<name>;` per slot; the backend binds slot `i` to
||| texture unit `i`.
public export
glslTextureDecls : List String -> String
glslTextureDecls = concatMap (\n => "uniform sampler2D t_" ++ n ++ ";\n")

||| GLSL insists `#version` come first, so the generated declarations go
||| after that line rather than in front of it.
public export
spliceAfterVersion : (decls : String) -> (src : String) -> String
spliceAfterVersion decls src = case lines src of
  (v :: rest) => unlines (v :: decls :: rest)
  [] => decls

||| A material's vertex stage: attributes, engine blocks, and the material
||| block (a vertex shader may read material data, as bevy's can).
public export
glslMaterialVert : (fields : List Field) -> (src : String) -> String
glslMaterialVert fields =
  spliceAfterVersion (glslVertexIn ++ glslEngineBlocks ++ glslBlock "Mat" fields)

||| A material's fragment stage: engine blocks, material block, textures,
||| and the alpha helper. The default precision comes first -- ES 300
||| requires it declared before any float appears, and the generated blocks
||| would otherwise land ahead of the authored body's own declaration.
||| (Repeating it in the body is legal and harmless.)
public export
glslMaterialFrag : (fields : List Field) -> (textures : List String) -> (src : String) -> String
glslMaterialFrag fields textures =
  spliceAfterVersion ("precision highp float;\n"
                        ++ glslEngineBlocks ++ glslBlock "Mat" fields
                        ++ glslTextureDecls textures ++ glslAlphaHelper)

public export
glslLineVert : (src : String) -> String
glslLineVert = spliceAfterVersion (glslLineVertexIn ++ glslEngineBlocks)

public export
glslLineFrag : (src : String) -> String
glslLineFrag = spliceAfterVersion ("precision highp float;\n" ++ glslEngineBlocks)

--------------------------------------------------------------------------------
-- Generated binding specs

||| Idris cannot build the nested descriptor a bind group layout wants, in
||| either JavaScript or C, so the backends parse a spec string once per
||| pipeline instead of each declaring the entries itself. One record per
||| entry, semicolon-separated; the first integer is the kind:
|||
|||   0,slot,visibility,dynamic,minBindingSize   -- a uniform buffer
|||   1,slot                                     -- a texture (fragment)
|||   2,slot                                     -- its sampler
public export
bufferEntry : (slot, visibility : Int) -> (dynamic : Bool) -> (size : Int) -> String
bufferEntry slot vis dyn size =
  "0," ++ show slot ++ "," ++ show vis ++ ","
    ++ (if dyn then "1" else "0") ++ "," ++ show size

engineEntries : (matSize : Int) -> List String
engineEntries matSize =
  [ bufferEntry 0 3 False (structSize globalFields)
  , bufferEntry 1 3 True (structSize objFields)
  , bufferEntry 2 3 True matSize
  ]

textureEntries : Int -> List String
textureEntries n = go (cast n) 0
  where
    go : Nat -> Int -> List String
    go Z _ = []
    go (S k) i =
      ("1," ++ show (3 + 2 * i)) :: ("2," ++ show (4 + 2 * i)) :: go k (i + 1)

||| The bind group layout for a material pipeline: globals, the engine
||| block, the material block sized from its fields, and its textures.
public export
materialBindSpec : (fields : List Field) -> (texCount : Int) -> String
materialBindSpec fields texCount =
  joinSemi (engineEntries (structSize fields) ++ textureEntries texCount)

||| The line pipeline binds only the engine blocks.
public export
lineBindSpec : String
lineBindSpec =
  joinSemi [ bufferEntry 0 3 False (structSize globalFields)
           , bufferEntry 1 3 True (structSize objFields) ]

||| `location,byteOffset,components` per attribute, semicolon-separated.
public export
vertexSpecOf : List Field -> String
vertexSpecOf fields = joinSemi (go 0 0 fields)
  where
    go : Int -> Int -> List Field -> List String
    go _ _ [] = []
    go i at (MkField _ t :: fs) =
      (show i ++ "," ++ show at ++ "," ++ show (components t))
        :: go (i + 1) (at + sizeOf t) fs

public export
meshVertexSpec : String
meshVertexSpec = vertexSpecOf meshVertexFields

public export
lineVertexSpec : String
lineVertexSpec = vertexSpecOf lineVertexFields

--------------------------------------------------------------------------------
-- The constants, and the proofs that make them honest

||| Bytes per frame-global uniform binding.
public export
globalSize : Int
globalSize = 176

0 globalSizeOk : Offler.Gfx.Layout.globalSize = Offler.Gfx.Layout.structSize Offler.Gfx.Layout.globalFields
globalSizeOk = Refl

||| The same, in Float32Array elements, which is what the pokes take.
public export
globalFloats : Int
globalFloats = 44

0 globalFloatsOk : Offler.Gfx.Layout.globalFloats * 4 = Offler.Gfx.Layout.globalSize
globalFloatsOk = Refl

||| Float offsets inside the globals, for the pokes.
public export
camFloat : Int
camFloat = 32

0 camFloatOk : Offler.Gfx.Layout.camFloat * 4
             = Offler.Gfx.Layout.endAt 0 [MkField "proj" Mat4, MkField "view" Mat4]
camFloatOk = Refl

public export
lightDirFloat : Int
lightDirFloat = 36

0 lightDirFloatOk : Offler.Gfx.Layout.lightDirFloat * 4
                  = Offler.Gfx.Layout.endAt 0
                      [ MkField "proj" Mat4, MkField "view" Mat4
                      , MkField "cam" Vec3, MkField "time" F32 ]
lightDirFloatOk = Refl

public export
lightColorFloat : Int
lightColorFloat = 40

0 lightColorFloatOk : Offler.Gfx.Layout.lightColorFloat * 4
                    = Offler.Gfx.Layout.endAt 0
                        [ MkField "proj" Mat4, MkField "view" Mat4
                        , MkField "cam" Vec3, MkField "time" F32
                        , MkField "lightDir" Vec3, MkField "ambient" F32 ]
lightColorFloatOk = Refl

||| Bytes per engine object binding: the model matrix and the alpha lane.
public export
objSize : Int
objSize = 80

0 objSizeOk : Offler.Gfx.Layout.objSize = Offler.Gfx.Layout.structSize Offler.Gfx.Layout.objFields
objSizeOk = Refl

||| Float offset of the lane within the object block.
public export
objLaneFloat : Int
objLaneFloat = 16

0 objLaneOk : Offler.Gfx.Layout.objLaneFloat * 4
            = Offler.Gfx.Layout.endAt 0 [MkField "model" Mat4]
objLaneOk = Refl

||| Bytes between slots in the object and material buffers alike. A dynamic
||| offset must be a multiple of `minUniformBufferOffsetAlignment`, which is
||| 256 on every device that ships -- and GLSL's
||| `UNIFORM_BUFFER_OFFSET_ALIGNMENT` is at most 256 likewise, so one stride
||| serves all three backends. A material block must fit in a slot, which is
||| the `So (structSize (matFields {m}) <= 256)` bound `registerMaterial`
||| demands at compile time.
public export
objStride : Int
objStride = 256

0 objStrideOk : Offler.Gfx.Layout.objStride = Offler.Gfx.Layout.roundUp Offler.Gfx.Layout.objSize 256
objStrideOk = Refl

public export
objFloats : Int
objFloats = 64

0 objFloatsOk : Offler.Gfx.Layout.objFloats * 4 = Offler.Gfx.Layout.objStride
objFloatsOk = Refl

||| One object slot and one material slot per draw per frame, so this is the
||| ceiling on a frame's draw count. Two 2.6 MB buffers at 256 bytes a slot.
public export
maxObjects : Int
maxObjects = 10240

||| The most texture slots a material may declare, which is what the C
||| shim's fixed-arity draw call carries.
public export
maxTextureSlots : Int
maxTextureSlots = 4

public export
meshStride : Int
meshStride = 32

0 meshStrideOk : Offler.Gfx.Layout.meshStride = Offler.Gfx.Layout.packedEnd 0 Offler.Gfx.Layout.meshVertexFields
meshStrideOk = Refl

||| Floats per mesh vertex, which is what `Verts` is indexed by.
public export
meshFloats : Int
meshFloats = 8

0 meshFloatsOk : Offler.Gfx.Layout.meshFloats * 4 = Offler.Gfx.Layout.meshStride
meshFloatsOk = Refl

public export
lineStride : Int
lineStride = 16

0 lineStrideOk : Offler.Gfx.Layout.lineStride
               = Offler.Gfx.Layout.roundUp (Offler.Gfx.Layout.packedEnd 0 Offler.Gfx.Layout.lineVertexFields) 16
lineStrideOk = Refl

public export
lineFloats : Int
lineFloats = 4

0 lineFloatsOk : Offler.Gfx.Layout.lineFloats * 4 = Offler.Gfx.Layout.lineStride
lineFloatsOk = Refl
