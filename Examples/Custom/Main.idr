||| Entry point for the desktop build.
module Examples.Custom.Main

import Examples.Custom.Scene
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
  Just r <- initWgpu "Custom"
    | Nothing => putStrLn "custom: could not open a window or start wgpu."
  p <- initNative "Custom" (ctxOf r)
  putStrLn "custom: Esc quits."
  run r p (nativeStatus p)
