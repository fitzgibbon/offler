||| Entry point for the desktop build: the same `Scene` the browser runs, on
||| SDL3 and wgpu instead of the DOM and WebGPU.
module Examples.Lines.Main

import Examples.Lines.Scene
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Native.Platform
import Offler.Native.Wgpu

%default covering

main : IO ()
main = do
  Just r <- initWgpu "Lines"
    | Nothing => putStrLn "lines: could not open a window or start wgpu."
  p <- initNative "Lines" (ctxOf r)
  putStrLn "lines: Esc quits."
  run r p
