||| Must NOT compile: a frame that is begun and never ended leaves a linear
||| name with zero uses.
-- expect: uses of linear name
module Tests.DropFrame
import Offler.Camera
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Math

bad : Renderer r f => r -> Camera -> Lights -> L IO ()
bad rr cam ls = do
  Just fr <- beginFrame rr cam ls 0.0
    | Nothing => pure ()
  pure ()
