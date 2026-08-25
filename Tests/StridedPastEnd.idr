-- expect: Can't find an implementation for LTE 4 3
-- `strided` mints element positions from a proof `i < n`. Element 3 of a
-- 3-element run is one past the end, and `LT 3 3` has no proof -- where the
-- `window` test this replaced would have caught it at run time by silently
-- dropping the write.
module Tests.StridedPastEnd

import Offler.Gfx.Array

past : At (3 * 24) 24
past = strided 3 {n = 3} %search
