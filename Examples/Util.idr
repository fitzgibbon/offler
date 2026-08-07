||| The scraps every example wants: an fps counter and a mesh-upload
||| one-liner.
module Examples.Util

import Data.IORef
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
reportFps : Platform p => p -> FpsCounter -> Double -> IO ()
reportFps p fps t = do
  f <- readIORef fps.frames
  since <- readIORef fps.lastReport
  if t - since >= 0.5
    then do
      setStatus p "fps" (show (the Int (cast (cast f / (t - since) + 0.5))) ++ " fps")
      writeIORef fps.frames 0
      writeIORef fps.lastReport t
    else writeIORef fps.frames (f + 1)

||| Build and upload a primitive in one step.
export
loadMesh : Renderer r f => r -> MeshData -> IO MeshHandle
loadMesh r md = do
  buf <- uploadMesh md
  createMesh r buf.handle

||| Upload world-space segments in one step.
export
loadLines : Renderer r f => r -> List (V3, V3) -> IO ()
loadLines r segs = do
  buf <- uploadLines segs
  setLines r buf.handle
