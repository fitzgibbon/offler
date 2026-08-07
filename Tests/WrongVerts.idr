||| Must NOT compile: line vertices are padded to four floats and the mesh
||| pipeline reads six. The stride is in the type, so they cannot be swapped
||| even though both are flat float arrays.
-- expect: Mismatch between
module Tests.WrongVerts
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Renderer

bad : Renderer r f => r -> Verts Offler.Gfx.Layout.lineFloats -> IO ()
bad rr overlay = ignore (createMesh rr overlay)
