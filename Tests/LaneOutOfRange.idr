-- expect: Can't find an implementation for So (with block in integerLessThanNat 16 False 16)
-- A material may only write the sixteen lanes of its own slot. Lane 16 is
-- past the end; before the lane became a `Fin` this compiled and the write
-- was silently dropped, leaving the uniform block zeroed with no error.
module Tests.LaneOutOfRange

import Offler.Gfx.Material

past : MatWriter -> IO ()
past w = putVec4 w 16 0.0 0.0 0.0 0.0
