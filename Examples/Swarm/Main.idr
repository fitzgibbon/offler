||| Entry point for the desktop build: the same `Scene` the browser runs, on
||| SDL3 and wgpu instead of the DOM and WebGPU.
module Examples.Swarm.Main

import Examples.Swarm.Scene
import Examples.Util
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Native.Platform
import Offler.Native.Wgpu

-- `Offler.Gfx.Renderer` re-exports `Control.Linear.LIO`, whose binds make
-- plain IO do-blocks ambiguous here.
%hide Control.Linear.LIO.(>>=)
%hide Control.Linear.LIO.(>>)

%default covering

main : IO ()
main = do
  Just r <- initWgpu "Swarm"
    | Nothing => putStrLn "swarm: could not open a window or start wgpu."
  p <- initNative "Swarm" (ctxOf r)
  putStrLn "swarm: Esc quits."
  run r p (nativeStatus p)
