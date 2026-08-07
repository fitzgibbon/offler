||| Must NOT compile: the token is consumed by `endFrame`, so a draw after it
||| uses a linear name twice.
-- expect: uses of linear name
module Tests.DrawAfterEnd
import Offler.Camera
import Offler.Color
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights -> MeshHandle -> L IO ()
bad rr cam ls mesh = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  _   <- endFrame rr fr
  fr2 <- draw rr fr mesh identity (lit white)
  endFrame rr fr2
