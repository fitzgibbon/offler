||| The scraps every example wants: an fps counter and mesh-upload
||| one-liners. `Status` and `App` come from `Offler.App`, re-exported so
||| scenes keep a single utility import.
module Examples.Util

import Data.IORef
import public Offler.App
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Math
import Offler.Mesh

%default covering

public export
record FpsCounter where
  constructor MkFpsCounter
  frames : IORef Int
  lastReport : IORef Double

export
newFps : IO FpsCounter
newFps = MkFpsCounter <$> newIORef 0 <*> newIORef 0.0

||| Frame rate, averaged over roughly half a second, into the "fps" status
||| slot.
export
reportFps : Status -> FpsCounter -> Double -> IO ()
reportFps status fps t = do
  f <- readIORef fps.frames
  since <- readIORef fps.lastReport
  if t - since >= 0.5
    then do
      status "fps" (show (the Int (cast (cast f / (t - since) + 0.5))) ++ " fps")
      writeIORef fps.frames 0
      writeIORef fps.lastReport t
    else writeIORef fps.frames (f + 1)

||| Build and upload a triangle-soup primitive in one step.
export
loadMesh : Renderer r f => r -> MeshData -> IO (MeshHandle Triangles)
loadMesh r md = do
  buf <- uploadMesh md
  createMesh r buf.handle

||| Build and upload an indexed mesh in one step.
export
loadIndexed : Renderer r f => r -> (List Vertex, List Int)
           -> IO (MeshHandle Triangles)
loadIndexed r (vs, is) = do
  (buf, ix) <- uploadIndexed vs is
  createMeshIndexed r buf.handle ix

