||| Entry point for the desktop build: the same `Scene` the browser runs, on
||| SDL3 and wgpu instead of the DOM and WebGPU.
module Examples.Swarm.Main

import Examples.Swarm.Scene
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Native.Platform
import Offler.Native.Wgpu
import Offler.Shaders

%default covering

main : IO ()
main = do
  Just r <- initWgpu "Swarm" wgslSrc
    | Nothing => putStrLn "swarm: could not open a window or start wgpu."
  p <- initNative "Swarm" (ctxOf r)
  putStrLn "swarm: Esc quits."
  run r p
