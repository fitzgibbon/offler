||| Entry point for the WebGPU bundle, compiled separately from the WebGL2
||| one so neither bundle carries the other's backend.
module Examples.Fountain.MainGpu

import Examples.Fountain.Scene
import Offler.Web.App

%default covering

main : IO ()
main = launchGpu app
