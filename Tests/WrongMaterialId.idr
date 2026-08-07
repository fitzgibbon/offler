||| Must NOT compile: a `MaterialId` is phantom-typed by the material it was
||| registered for, so the standard material's pipelines cannot be handed a
||| hologram's data. Pairing them is a type error, not a wrong image.
-- expect: Mismatch between: Hologram and StandardMaterial
module Tests.WrongMaterialId
import Examples.Custom.Scene
import Offler.Camera
import Offler.Color
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights
   -> MaterialId StandardMaterial -> MeshHandle -> L IO ()
bad rr cam ls stdId mesh = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  fr2 <- draw rr fr stdId mesh identity (MkHologram white 8.0)
  endFrame rr fr2
