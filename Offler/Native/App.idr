||| The desktop launcher: an SDL3 window, the wgpu renderer and the SDL3
||| mixer, assembled around any `App`.
module Offler.Native.App

import Data.String
import public Offler.App
import Offler.Native.Audio
import Offler.Native.Platform
import Offler.Native.Wgpu

-- `Offler.App` re-exports `Offler.Gfx.Renderer`, which re-exports
-- `Control.Linear.LIO`, whose binds make plain IO do-blocks ambiguous.
%hide Control.Linear.LIO.(>>=)
%hide Control.Linear.LIO.(>>)

%default covering

export
launch : App -> IO ()
launch app = do
  let name = toLower app.title
  Just r <- initWgpu app.title
    | Nothing => putStrLn (name ++ ": could not open a window or start wgpu.")
  p <- initNative app.title (ctxOf r)
  au <- initAudio
  putStrLn (name ++ ": Esc quits.")
  app.scene r p au (nativeStatus p)
