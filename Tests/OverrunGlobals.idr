||| Must NOT compile: the frame globals are 72 floats, so a mat4 written at
||| offset 60 would run four floats past the end. `here` demands the bound.
-- expect: Can't find an implementation for LTE (plus 60 16) globalFloatsN
module Tests.OverrunGlobals
import Offler.Gfx.Array
import Offler.Gfx.Uniform
import Offler.Math

bad : GlobalScratch -> Mat4 -> IO ()
bad a m = pokeMat a (here 60) m
