||| Entry point for the desktop build: the same `Scene` the browser runs, on
||| SDL3 and wgpu instead of the DOM and WebGPU -- and the SDL3 mixer where
||| the browser has Web Audio.
module Examples.Chime.Main

import Examples.Chime.Scene
import Examples.Util
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Native.Audio
import Offler.Native.Platform
import Offler.Native.Wgpu

-- `Offler.Gfx.Renderer` re-exports `Control.Linear.LIO`, whose binds make
-- plain IO do-blocks ambiguous here.
%hide Control.Linear.LIO.(>>=)
%hide Control.Linear.LIO.(>>)

%default covering

main : IO ()
main = do
  Just r <- initWgpu "Chime"
    | Nothing => putStrLn "chime: could not open a window or start wgpu."
  p <- initNative "Chime" (ctxOf r)
  au <- initAudio
  putStrLn "chime: Esc quits."
  run r p au (nativeStatus p)
