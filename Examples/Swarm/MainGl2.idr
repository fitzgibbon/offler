||| Entry point for the WebGL2 bundle. The backend is fixed at compile time,
||| so this build contains no WebGPU code at all and the renderer is
||| monomorphic; the footer button navigates to the other bundle rather than
||| switching anything at run time.
module Examples.Swarm.MainGl2

import Examples.Swarm.Scene
import Examples.Util
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Web.Gl2
import Offler.Web.Js
import Offler.Web.Platform

-- `Offler.Gfx.Renderer` re-exports `Control.Linear.LIO`, whose binds make
-- plain IO do-blocks ambiguous here.
%hide Control.Linear.LIO.(>>=)
%hide Control.Linear.LIO.(>>)

%default covering

main : IO ()
main = do
  onUncaught showError
  setText "flip" "Switch to WebGPU"
  gpuOk <- hasWebGPU
  if gpuOk
    then onClick "flip" (setSearch "?r=webgpu")
    else do disable "flip"
            setText "note" "WebGPU unavailable in this browser"
  p <- initWeb "gl"
  r <- initGl2 "gl"
  run r p webStatus
