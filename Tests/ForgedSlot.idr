||| Must NOT compile: a slot is only ever made by `slot`, which is where the
||| bounds test is. Nothing outside `Offler.Gfx.Uniform` can build one.
-- expect: MkSlot is private
module Tests.ForgedSlot
import Offler.Gfx.Array
import Offler.Gfx.Uniform
import Offler.Math

bad : ObjScratch -> Mat4 -> IO ()
bad a m = pokeObject a (MkSlot (here 0)) m 0.0 0.0 0.0 0.0
