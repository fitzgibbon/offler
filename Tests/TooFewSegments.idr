-- expect: Can't find an implementation for LTE 3 2
-- A circle needs at least three segments. This used to compile: the builders
-- took an `Int` and opened with `max 3 segments`, so asking for two silently
-- got you three, with nothing saying the argument had been overridden.
module Tests.TooFewSegments

import Offler.Mesh

degenerate : MeshData 2
degenerate = circle 1.0 2
