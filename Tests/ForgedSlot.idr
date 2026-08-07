||| Must NOT compile: a slot is only ever made by `slot`, which is where the
||| bounds test is. Nothing outside `Offler.Gfx.Uniform` can build one.
-- expect: MkSlot is private
module Tests.ForgedSlot
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Renderer
import Offler.Gfx.Uniform
import Offler.Material
import Offler.Math

bad : ObjScratch -> Mat4 -> Material -> IO ()
bad a m mat = pokeObject a (MkSlot (here 0)) m mat
