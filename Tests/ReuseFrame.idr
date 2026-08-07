||| Must NOT compile: drawing twice from the same token forks the pass.
-- expect: uses of linear name
module Tests.ReuseFrame
import Offler.Camera
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights
   -> MeshHandle Triangles -> Handle StandardMaterial -> L IO ()
bad rr cam ls mesh h = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  a <- draw rr fr mesh h identity
  b <- draw rr fr mesh h identity
  _ <- endFrame rr a
  endFrame rr b
