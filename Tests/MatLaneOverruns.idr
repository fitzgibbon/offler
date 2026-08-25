-- expect: Can't find an implementation for So (with block in integerLessThanNat 13 False 13)
-- A mat4 spans four lanes, so lane 12 is the last legal start. Lane 13
-- would run off the end of the slot; the old `lane + 3 < 16` test dropped
-- the write instead.
module Tests.MatLaneOverruns

import Offler.Gfx.Material
import Offler.Math

past : MatWriter -> Mat4 -> IO ()
past w m = putMat4 w 13 m
