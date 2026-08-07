||| Must NOT compile: an instanced draw may only use a material that
||| declares its instanced entry points (`matInstEntry`), which the
||| Hologram does not. `InstOk` demands the proof at the draw site, the
||| same compile-time gate as `TopoOk`.
-- expect: Can't find an implementation for So
module Tests.InstNeedsEntry
import Examples.Custom.Scene
import Offler.Camera
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights
   -> MeshHandle Triangles -> Handle Hologram -> InstanceHandle -> L IO ()
bad rr cam ls mesh h ih = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  fr2 <- drawInstanced rr fr mesh h ih identity
  endFrame rr fr2
