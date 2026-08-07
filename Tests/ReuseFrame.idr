||| Must NOT compile: drawing twice from the same token forks the pass.
-- expect: uses of linear name
module Tests.ReuseFrame
import Offler.Camera
import Offler.Color
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights
   -> MaterialId StandardMaterial -> MeshHandle -> L IO ()
bad rr cam ls mid mesh = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  a <- draw rr fr mid mesh identity (lit white)
  b <- draw rr fr mid mesh identity (lit white)
  _ <- endFrame rr a
  endFrame rr b
