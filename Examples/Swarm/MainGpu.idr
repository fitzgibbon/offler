||| Entry point for the WebGPU bundle, compiled separately from the WebGL2
||| one so neither bundle carries the other's backend.
module Examples.Swarm.MainGpu

import Examples.Swarm.Scene
import Offler.Web.App

%default covering

main : IO ()
main = launchGpu app
