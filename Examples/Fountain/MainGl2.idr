||| Entry point for the WebGL2 bundle. The backend is still fixed at compile
||| time -- `launchGl2` owns the page chrome and backend construction.
module Examples.Fountain.MainGl2

import Examples.Fountain.Scene
import Offler.Web.App

%default covering

main : IO ()
main = launchGl2 app
