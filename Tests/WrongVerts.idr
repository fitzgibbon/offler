||| Must NOT compile: topology is a property of the mesh, in the type --
||| line vertices cannot be uploaded as a triangle mesh.
-- expect: Mismatch between
module Tests.WrongVerts
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Renderer

bad : Renderer r f => r -> Verts Lines -> IO (MeshHandle Triangles)
bad rr overlay = createMesh rr overlay
