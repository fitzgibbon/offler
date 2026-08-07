||| Entry point for the desktop build: the same `Scene` the browser runs, on
||| SDL3 and wgpu instead of the DOM and WebGPU.
module Examples.Shapes.Main

import Examples.Shapes.Scene
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Native.Platform
import Offler.Native.Wgpu

%default covering

main : IO ()
main = do
  Just r <- initWgpu "Shapes"
    | Nothing => putStrLn "shapes: could not open a window or start wgpu."
  p <- initNative "Shapes" (ctxOf r)
  putStrLn "shapes: Esc quits."
  run r p
