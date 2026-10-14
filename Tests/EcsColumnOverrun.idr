-- expect: Can't find an implementation for LTE (S (S scalarFloats)) 2
-- A packed component writes its fields at positions `sub` proves inside
-- its declared width. Two floats wide, a third field does not fit, and
-- the store never sees the overrun into its neighbour's slot.
module Tests.EcsColumnOverrun

import Offler.Ecs

record V3ish where
  constructor MkV3ish
  x, y, z : Double

Columnar V3ish where
  width = 2
  storeCol a o (MkV3ish x y z) = do
    poke a (sub 0 o) x; poke a (sub 1 o) y; poke a (sub 2 o) z
  loadCol a o = [| MkV3ish (peek a (sub 0 o)) (peek a (sub 1 o)) (pure 0.0) |]
