-- expect: Can't find an implementation for Write
-- Writes are resolved against the world's component list too: a system
-- cannot insert a component the world has no store for.
module Tests.EcsWriteMissing

import Language.Reflection
import Offler.Ecs

%language ElabReflection

record Pos where
  constructor MkPos
  px : Double

record Vel where
  constructor MkVel
  vx : Double

Component Pos where kind = Boxed
Component Vel where kind = Boxed

%runElab world "PosOnly" [`(Pos)]


launch : World PosOnly -> IO ()
launch w = cmap {q = Pos} {r = Vel} w (\(MkPos p) => MkVel p)
