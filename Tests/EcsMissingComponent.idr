-- expect: Can't find an implementation for Query
-- A world's component list is in its type, so a system that queries a
-- component the world was not built with has no `Query` instance --
-- where bevy panics at run time on an unregistered component.
module Tests.EcsMissingComponent

import Offler.Ecs

record Pos where
  constructor MkPos
  px : Double

record Vel where
  constructor MkVel
  vx : Double

Component Pos where kind = Boxed
Component Vel where kind = Boxed

step : World [Pos] -> IO ()
step w = cmap {q = (Pos, Vel)} w (\(MkPos p, MkVel v) => MkPos (p + v))
