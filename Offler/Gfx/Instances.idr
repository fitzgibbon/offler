||| CPU-side instance data: what an instanced draw streams per instance --
||| a model matrix and a colour, `instanceFloats` apiece in the layout
||| `Offler.Gfx.Layout.instanceFields` describes.
|||
||| The staging buffer is persistent: allocate one big enough once, refill
||| it each frame with `fillInstances`, and hand the returned slice to
||| `writeInstances`. The fill loop is the same trampolined `PrimIO` as
||| `fillWith` -- the world bound on the left-hand side, so the JS backend
||| gets a saturated self tail call -- because a crowd's fill is a hot
||| path at fifty thousand instances a frame.
module Offler.Gfx.Instances

import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Math

%default total

||| A filled prefix of a staging buffer: what `writeInstances` uploads.
||| The constructor is private -- `fillInstances` is the only mint, and its
||| loop bounds the count by the buffer's capacity.
export
data InstSlice : Type where
  MkInstSlice : AnyPtr -> (count : Int) -> InstSlice

||| For backends only.
export %inline
instRaw : InstSlice -> AnyPtr
instRaw (MkInstSlice p _) = p

||| Instances in the slice, which is what an instanced draw counts.
export %inline
instCount : InstSlice -> Int
instCount (MkInstSlice _ n) = n

||| A persistent staging buffer for up to `capInst` instances.
export
record InstBuf where
  constructor MkInstBuf
  0 bufCap : Nat
  arr : F32Array bufCap
  capInst : Int

export
newInstBuf : (capInst : Int) -> IO InstBuf
newInstBuf n = do
  let k = max 1 n
      floats = the Nat (cast (k * instanceFloats))
  a <- newF32 floats
  pure (MkInstBuf floats a k)

pokeOne : F32Array cap -> Int -> Mat4 -> Color -> IO ()
pokeOne a i m c = do
  pokeMat a (unsafeAt (i * instanceFloats)) m
  poke4 a (unsafeAt (i * instanceFloats + instColorFloat))
        c.red c.green c.blue c.alpha

go : F32Array cap -> (limit : Int) -> (x -> (Mat4, Color))
  -> Int -> List x -> PrimIO Int
go a limit f i [] w = MkIORes i w
go a limit f i (v :: rest) w =
  if i >= limit
    then MkIORes i w
    else let (m, c) = f v in
         case toPrim (pokeOne a i m c) w of
           MkIORes _ w' => go a limit f (i + 1) rest w'

||| Fill from the start: one model matrix and colour per item, capped at
||| the buffer's capacity. Takes the projection function rather than a
||| pre-mapped list, so no intermediate fifty-thousand-cons list is built
||| per frame -- and no non-tail `map` recursion overflows building one.
export
fillInstances : InstBuf -> (x -> (Mat4, Color)) -> List x -> IO InstSlice
fillInstances b f xs = do
  n <- fromPrim (go b.arr b.capInst f 0 xs)
  pure (MkInstSlice (raw b.arr) n)
