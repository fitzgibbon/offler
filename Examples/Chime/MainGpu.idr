||| Entry point for the WebGPU bundle. Compiled separately from the WebGL2
||| one, so neither bundle carries the other's backend. Device acquisition is
||| asynchronous, which is why `run` is reached from inside a continuation
||| here and directly in the WebGL2 build.
module Examples.Chime.MainGpu

import Examples.Chime.Scene
import Examples.Util
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Web.Audio
import Offler.Web.Gpu
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
  setText "flip" "Switch to WebGL2"
  onClick "flip" (setSearch "?r=webgl2")
  gpuOk <- hasWebGPU
  if not gpuOk
    then showError "WebGPU is not available here. Switch to the WebGL2 build."
    else do
      p <- initWeb "gl"
      au <- initAudio
      initGpu "gl" $ \res =>
        case res of
          Just gpu => run gpu p au webStatus
          Nothing => showError "navigator.gpu exists but no adapter was available."
