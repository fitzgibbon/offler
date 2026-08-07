||| Must NOT compile: a line-topology mesh may only draw with a material
||| that declares its line entry points (`matLineEntry`), which the
||| Hologram does not. `TopoOk` demands the proof at the draw site -- what
||| bevy discovers as a runtime pipeline-specialisation failure.
-- expect: Can't find an implementation for So
module Tests.LineNeedsEntry
import Examples.Custom.Scene
import Offler.Camera
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights
   -> MeshHandle Lines -> Handle Hologram -> L IO ()
bad rr cam ls mesh h = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  fr2 <- draw rr fr mesh h identity
  endFrame rr fr2
