||| One description of everything the CPU and the GPU have to agree about.
|||
||| These contracts would otherwise be asserted independently in up to four
||| places: a `struct` and a `@group/@binding` line in the WGSL, a
||| `layout(location=)` in the GLSL, a `#define` in the C shim, and a
||| hand-counted constant in each Idris backend. Nothing would check them
||| against each other, and disagreement shows up either as a validation error
||| at pipeline creation or -- worse -- as silently misread fields.
|||
||| Here the description is the source. The shader text is generated from it,
||| the WebGPU-flavoured backends are handed the layout as spec strings they parse
||| once at startup, and the Idris offsets are **literals proved equal to the
||| computed layout**. That last part is what keeps it free: `structSize` and
||| friends run in the type checker, and what survives to run time is the
||| integer you would have written by hand anyway. Get one wrong and the module
||| does not compile.
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

public export
wgslStruct : String -> List Field -> String
wgslStruct nm fs = "struct " ++ nm ++ " {\n" ++ concatMap line fs ++ "};\n"
  where
    line : Field -> String
    line (MkField n t) = "  " ++ n ++ " : " ++ wgslTy t ++ ",\n"

--------------------------------------------------------------------------------
-- Bindings

public export
data Stage = Vert | Frag | Both

||| The `GPUShaderStage` bitmask. The browser and `webgpu.h` agree on it:
||| vertex is 1, fragment is 2.
public export
stageMask : Stage -> Int
stageMask Vert = 1
stageMask Frag = 2
stageMask Both = 3

||| A uniform block: the struct, the WGSL name it is bound to, and how the
||| pipeline must declare it. Everything about it is derived from here -- the
||| struct declaration, the `@group(0) @binding(n)` line, the bind group
||| layout entry and its `minBindingSize`.
public export
record Binding where
  constructor MkBinding
  slot : Int
  var : String
  structName : String
  fields : List Field
  dynamic : Bool
  stages : Stage

public export
bindingSize : Binding -> Int
bindingSize (MkBinding _ _ _ fs _ _) = structSize fs

public export
wgslBinding : Binding -> String
wgslBinding (MkBinding s v n fs _ _) =
  wgslStruct n fs
    ++ "@group(0) @binding(" ++ show s ++ ") var<uniform> " ++ v ++ " : " ++ n ++ ";\n"

||| `slot,visibility,dynamic,minBindingSize`, semicolon-separated. Idris cannot
||| build the nested descriptor a bind group layout wants, in either JavaScript
||| or C, so the backends parse this once at startup instead of each declaring
||| the entries itself.
public export
bindingEntry : Binding -> String
bindingEntry b@(MkBinding s _ _ _ dyn st) =
  show s ++ "," ++ show (stageMask st) ++ ","
    ++ (if dyn then "1" else "0") ++ "," ++ show (bindingSize b)

--------------------------------------------------------------------------------
-- What offler's standard pipeline uses

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

public export
objFields : List Field
objFields =
  [ MkField "model" Mat4
  , MkField "baseColor" Vec4
  , MkField "emissive" Vec4      -- rgb, w = shading mode
  , MkField "params" Vec4        -- metallic, roughness, unused, unused
  ]

public export
bindings : List Binding
bindings =
  [ MkBinding 0 "g" "Globals" globalFields False Both
  , MkBinding 1 "o" "Obj" objFields True Both
  ]

||| The mesh vertex: a position and a normal, packed tightly.
public export
meshVertexFields : List Field
meshVertexFields = [MkField "pos" Vec3, MkField "normal" Vec3]

||| The line vertex: a position padded out to 16 bytes so `poke16` fills four
||| vertices per foreign call. Lines have their own vertex-shader entry point,
||| so no normal attribute needs faking.
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
-- Generated shader text

public export
wgslVertexStruct : String -> List Field -> String
wgslVertexStruct nm fs = "struct " ++ nm ++ " {\n" ++ go 0 fs ++ "};\n"
  where
    go : Int -> List Field -> String
    go _ [] = ""
    go i (MkField n t :: fs) =
      "  @location(" ++ show i ++ ") " ++ n ++ " : " ++ wgslTy t ++ ",\n"
        ++ go (i + 1) fs

||| Prepended to the authored WGSL, so no shader can disagree with the offsets
||| that were proved below. `VertexIn` feeds the `vs` entry point, `LineIn`
||| feeds `vs_line`.
public export
wgslPrologue : String
wgslPrologue =
  wgslVertexStruct "VertexIn" meshVertexFields
    ++ wgslVertexStruct "LineIn" lineVertexFields
    ++ concatMap wgslBinding bindings

public export
glslVertexIn : String
glslVertexIn = go 0 meshVertexFields
  where
    go : Int -> List Field -> String
    go _ [] = ""
    go i (MkField n t :: fs) =
      "layout(location=" ++ show i ++ ") in " ++ glslTy t ++ " " ++ n ++ ";\n"
        ++ go (i + 1) fs

||| GLSL insists `#version` come first, so the generated declarations go after
||| that line rather than in front of it.
public export
withGlslPrologue : String -> String
withGlslPrologue src = case lines src of
  (v :: rest) => unlines (v :: glslVertexIn :: rest)
  [] => glslVertexIn

--------------------------------------------------------------------------------
-- Generated specs

public export
bindingSpec : String
bindingSpec = joinSemi (map bindingEntry bindings)

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

||| Bytes per object uniform binding.
public export
objSize : Int
objSize = 112

0 objSizeOk : Offler.Gfx.Layout.objSize = Offler.Gfx.Layout.structSize Offler.Gfx.Layout.objFields
objSizeOk = Refl

||| Offsets in Float32Array elements.
public export
objBaseColorFloat : Int
objBaseColorFloat = 16

0 objBaseColorOk : Offler.Gfx.Layout.objBaseColorFloat * 4
                 = Offler.Gfx.Layout.endAt 0 [MkField "model" Mat4]
objBaseColorOk = Refl

public export
objEmissiveFloat : Int
objEmissiveFloat = 20

0 objEmissiveOk : Offler.Gfx.Layout.objEmissiveFloat * 4
                = Offler.Gfx.Layout.endAt 0
                    [MkField "model" Mat4, MkField "baseColor" Vec4]
objEmissiveOk = Refl

public export
objParamsFloat : Int
objParamsFloat = 24

0 objParamsOk : Offler.Gfx.Layout.objParamsFloat * 4
              = Offler.Gfx.Layout.endAt 0
                  [ MkField "model" Mat4, MkField "baseColor" Vec4
                  , MkField "emissive" Vec4 ]
objParamsOk = Refl

||| Bytes between object slots in the one big uniform buffer. A dynamic offset
||| must be a multiple of `minUniformBufferOffsetAlignment`, which is 256 on
||| every device that ships, so a slot is an object rounded up to that.
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

||| One slot per object per frame, so this is the ceiling on a frame's draw
||| count. At 256 bytes a slot that is a 2.6 MB uniform buffer, well inside
||| `maxBufferSize`; only the 112-byte *binding* is subject to
||| `maxUniformBufferBindingSize`.
public export
maxObjects : Int
maxObjects = 10240

public export
meshStride : Int
meshStride = 24

0 meshStrideOk : Offler.Gfx.Layout.meshStride = Offler.Gfx.Layout.packedEnd 0 Offler.Gfx.Layout.meshVertexFields
meshStrideOk = Refl

||| Floats per mesh vertex, which is what `Verts` is indexed by.
public export
meshFloats : Int
meshFloats = 6

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
