||| Must NOT compile: `MaterialId` is phantom-typed by the material it was
||| registered for, so an asset of one material type cannot be created
||| against another type's pipelines.
-- expect: Mismatch between: Hologram and StandardMaterial
module Tests.WrongMaterialId
import Examples.Custom.Scene
import Offler.Color
import Offler.Gfx.Renderer
import Offler.Material

bad : Renderer r f => r -> MaterialId StandardMaterial
   -> IO (Handle StandardMaterial)
bad rr stdId = addMaterial rr stdId (MkHologram white 8.0)
