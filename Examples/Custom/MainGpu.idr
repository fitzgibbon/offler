||| Entry point for the WebGPU bundle.
module Examples.Custom.MainGpu

import Examples.Custom.Scene
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Web.Gpu
import Offler.Web.Js
import Offler.Web.Platform

%default covering

main : IO ()
main = do
  onUncaught showError
  setText "flip" "Switch to WebGL2"
  onClick "flip" (setSearch "?r=webgl2")
  gpuOk <- hasWebGPU
  if not gpuOk
    then showError "WebGPU is not available here. Switch to the WebGL2 build."
    else do
      p <- initWeb "gl"
      initGpu "gl" $ \res =>
        case res of
          Just gpu => run gpu p
          Nothing => showError "navigator.gpu exists but no adapter was available."
