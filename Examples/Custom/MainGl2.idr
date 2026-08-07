||| Entry point for the WebGL2 bundle. The backend is still fixed at compile
||| time -- `launchGl2` owns the page chrome and backend construction.
module Examples.Custom.MainGl2

import Examples.Custom.Scene
import Offler.Web.App

%default covering

main : IO ()
main = launchGl2 app
