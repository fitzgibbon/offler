||| The browser launchers, one per bundle. Both assume offler's standard
||| page: a canvas `#gl`, and optionally `#flip`, `#note` and `#error` --
||| every chrome helper no-ops when its element is missing. Each bundle is
||| a separate compilation reaching only its own launcher, so dead-code
||| elimination keeps WebGPU out of the WebGL2 bundle and vice versa.
module Offler.Web.App

import public Offler.App
import Offler.Web.Audio
import Offler.Web.Gl2
import Offler.Web.Gpu
import Offler.Web.Js
import Offler.Web.Platform

-- `Offler.App` re-exports `Offler.Gfx.Renderer`, which re-exports
-- `Control.Linear.LIO`, whose binds make plain IO do-blocks ambiguous.
%hide Control.Linear.LIO.(>>=)
%hide Control.Linear.LIO.(>>)

%default covering

export
launchGl2 : App -> IO ()
launchGl2 app = do
  onUncaught showError
  setText "flip" "Switch to WebGPU"
  gpuOk <- hasWebGPU
  if gpuOk
    then onClick "flip" (setSearch "?r=webgpu")
    else do disable "flip"
            setText "note" "WebGPU unavailable in this browser"
  p <- initWeb "gl"
  au <- initAudio
  r <- initGl2 "gl"
  app.scene r p au webStatus

||| Device acquisition is asynchronous, which is why the scene is reached
||| from inside a continuation here and directly in `launchGl2`.
export
launchGpu : App -> IO ()
launchGpu app = do
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
          Just gpu => app.scene gpu p au webStatus
          Nothing => showError "navigator.gpu exists but no adapter was available."
