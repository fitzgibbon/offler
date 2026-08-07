||| Must NOT compile: a material's uniform block must fit its 256-byte slot,
||| which `registerMaterial` demands a compile-time proof of. Five mat4s are
||| 320 bytes.
-- expect: Can't find an implementation for So
module Tests.FatMaterial
import Offler.Gfx.Renderer

data Fat = MkFat

public export
Material Fat where
  matFields = [ MkField "a" Mat4, MkField "b" Mat4, MkField "c" Mat4
              , MkField "d" Mat4, MkField "e" Mat4 ]
  matTextureSlots = []
  matWgsl = ""
  matGlslVert = ""
  matGlslFrag = ""
  alphaMode _ = Opaque
  matTextures _ = []
  writeMat _ _ = pure ()

bad : Renderer r f => r -> IO (MaterialId Fat)
bad rr = registerMaterial {m = Fat} rr
