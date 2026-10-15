||| Entry point for the desktop build: `Offler.Native.App.launch` owns the
||| window, renderer and mixer; only the scene is this module's.
module Examples.Racer.Main

import Examples.Racer.Scene
import Offler.Native.App

%default covering

main : IO ()
main = launch app
