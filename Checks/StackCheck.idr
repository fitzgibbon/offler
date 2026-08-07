||| Runs the batched uniform fill at exactly `maxObjects` on the node
||| backend, where the stack is the scarcest. This is the §8d.7 check from
||| the orrery's notes made permanent: whether the loop got the trampoline
||| is visible neither in the source nor in the types, and the first
||| version of `pokeObjects` overflowed V8 at exactly this count while
||| every other test passed. `make check` also greps the generated output
||| for `__tailRec`, so a regression fails twice.
module Checks.StackCheck

import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Uniform
import Offler.Material
import Offler.Math

%default covering

main : IO ()
main = do
  os <- newObjScratch
  let batch = map (\i => (translate (cast i) 0.0 0.0, lit white)) (range 0 (maxObjects - 1))
  next <- pokeObjects os 0 batch
  putStrLn ("filled " ++ show next ++ " of " ++ show maxObjects)
