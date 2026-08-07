||| Entry point for the WebGL2 bundle.
module Examples.Custom.MainGl2

import Examples.Custom.Scene
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Web.Gl2
import Offler.Web.Js
import Offler.Web.Platform

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
  run r p
