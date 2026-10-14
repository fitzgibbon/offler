-- expect: Can't find an implementation for Resource
-- `resource` reads a world-wide value, so it demands a `Resource`
-- instance -- which is what makes the component's store a global. An
-- ordinary component has many values, one per entity, and asking for
-- "its" value does not compile.
module Tests.EcsNotResource

import Offler.Ecs

record Pos where
  constructor MkPos
  px : Double

Component Pos where kind = Boxed

oops : World [Pos] -> IO Pos
oops w = resource w
