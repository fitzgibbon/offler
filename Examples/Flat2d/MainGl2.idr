||| Entry point for the WebGL2 bundle. The backend is still fixed at compile
||| time -- `launchGl2` owns the page chrome and backend construction.
module Examples.Flat2d.MainGl2

import Examples.Flat2d.Scene
import Offler.Web.App

%default covering

main : IO ()
main = launchGl2 app
