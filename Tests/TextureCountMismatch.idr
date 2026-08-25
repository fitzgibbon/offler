-- expect: Mismatch between
-- A material's `matTextures` must hand back exactly as many entries as it
-- declared slots. Before both became `Vect matTexCount`, declaring two
-- slots and returning one texture compiled, and the second slot silently
-- bound the renderer's 1x1 white texture.
module Tests.TextureCountMismatch

import Offler.Gfx.Material
import Offler.Color

record TwoSlots where
  constructor MkTwoSlots
  tex : Maybe TextureHandle

Material TwoSlots where
  matFields = [MkField "tint" Vec4]
  matTexCount = 2
  matTextureSlots = ["base", "detail"]
  matWgsl = ""
  matGlslVert = ""
  matGlslFrag = ""
  alphaMode _ = Opaque
  matTextures v = [v.tex]        -- one entry, two slots declared
  writeMat w _ = pure ()
