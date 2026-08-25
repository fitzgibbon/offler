||| The proof-carrying upload loops must survive V8 at crowd scale: their
||| recursion is `PrimIO` with the world on the left-hand side, and losing
||| that form (an ordinary IO do-block) overflows the stack past ~65 000
||| elements -- silently absent from the source, which is why this runs on
||| every `make check`, like `StackCheck` does for the batch fill.
module Main

import Data.Vect
import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Math
import Offler.Mesh
import Offler.Vect

main : IO ()
main = do
  let segs = tabulate 100000 (\i => (zero3, one3, white))
  lb <- uploadLines segs
  let tris = tabulate 100000 (\i => (MkVertex zero3 zero3 (MkV2 0.0 0.0)
                                    , MkVertex one3 zero3 (MkV2 0.0 0.0)
                                    , MkVertex zero3 one3 (MkV2 0.0 0.0)))
  mb <- uploadMesh tris
  -- The final vertex of the final triangle is `MkVertex zero3 one3 _`, so
  -- its normal.y -- float 4 of the last 8-float vertex -- must read back
  -- 1.0: a nonzero sentinel proving the writes reached the buffer's end.
  case window {w = 8} mb.arr (2400000 - 8) of
    Nothing => putStrLn "END WINDOW FAILED"
    Just o => do
      py <- peek mb.arr (sub 4 o)
      putStrLn ("lines " ++ show (vertsCount lb.handle)
             ++ " mesh " ++ show (vertsCount mb.handle)
             ++ " lastY " ++ show py)
