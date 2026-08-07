||| A flat array of 32-bit floats, shared by every backend, and the two things
||| that keep writes to it inside it.
|||
||| One declaration per primitive carrying a specifier per backend -- the
||| pattern `Data.Buffer` uses. The handle crosses the FFI as `AnyPtr`: a
||| `Float32Array` in the browser, a `malloc`'d `float *` natively. It cannot be
||| an `[external]` type, because those can be returned from a C function but
||| not passed to one.
|||
||| The array is indexed by its capacity in floats. The index is **erased**: it
||| exists for the type checker, and an `F32Array n` is still a pointer and a
||| length at run time. What it buys is that a position checked against one
||| array cannot be used to write into a smaller one.
module Offler.Gfx.Array

-- Re-exported: every caller of `here` or `sub` needs `Oh` in scope for the
-- bound to be discharged by proof search.
import public Data.So

%default total

||| Opaque so a vertex buffer cannot be confused with any other foreign handle,
||| and indexed so its capacity travels with it.
export
data F32Array : (0 cap : Int) -> Type where
  MkF32 : AnyPtr -> (len : Int) -> F32Array cap

||| Escape hatch for backends that hand the array to their own foreign calls.
export %inline
raw : F32Array cap -> AnyPtr
raw (MkF32 p _) = p

export %inline
capacity : F32Array cap -> Int
capacity (MkF32 _ n) = n

||| A place to write `w` floats, known to lie inside an array of capacity
||| `cap`. `MkAt` is private, so the only ways to get one are `here`, which
||| proves the bound, and `window`, which tests it -- there is no third way, and
||| every write below demands one.
|||
||| One `Int` field and one constructor, so Idris's newtype optimisation leaves
||| nothing of it at run time: an `At` is the offset.
export
data At : (0 cap : Int) -> (0 w : Int) -> Type where
  MkAt : Int -> At cap w

||| A literal offset into a known capacity. The bound is discharged by proof
||| search at multiplicity 0, so this costs nothing at all and an offset that
||| does not fit is a compile error.
export %inline
here : (i : Int) -> {auto 0 ok : So (i + w <= cap)} -> At cap w
here i = MkAt i

||| A computed offset: one comparison, made here so that no caller can make it
||| somewhere else, differently, or not at all. `Nothing` means the write would
||| overrun -- what to do about that is the caller's business, but it can no
||| longer be ignored.
||| Written `i <= n - w` rather than the obvious `i + w <= n`, because `Int`
||| wraps (at 64 bits on Chez and at 32 in the browser): for `i` within `w` of
||| `maxBound`, `i + w` comes out negative, compares below `n`, and the check
||| passes on a write that is off the end. `n - w` cannot overflow for
||| non-negative `n` and `w`, and goes negative when the array is smaller than
||| the write, which no non-negative `i` matches.
export
window : {w : Int} -> F32Array cap -> (i : Int) -> Maybe (At cap w)
window (MkF32 _ n) i = if i >= 0 && i <= n - w then Just (MkAt i) else Nothing

||| Step `k` floats into a window, keeping `w'` of it. Arithmetic on a bound
||| already established, not a second check, so this is free too.
export %inline
sub : (k : Int) -> At cap w -> {auto 0 ok : So (k + w' <= w)} -> At cap w'
sub k (MkAt i) = MkAt (i + k)

||| The offset a position names. There is no route back from an `Int` to an
||| `At` that skips the bound -- except the one named `unsafeAt`, below.
export %inline
atOffset : At cap w -> Int
atOffset (MkAt i) = i

||| Whether run-time bounds tests run. `Checked` is the default; `Trusted`
||| exists for release builds whose draw counts are bounded by construction.
||| The value the library was compiled with lives in `Offler.Gfx.Config`,
||| which the Makefile generates: `make BOUNDS=trusted` turns the tests off.
public export
data Bounds = Checked | Trusted

||| The third way, named as such: a position asserted rather than proved by
||| `here` or tested by `window`. It exists for two callers -- the batch
||| loops that test a whole run once and then step through it, and `Trusted`
||| builds -- and using it anywhere else forfeits what `At` promises. It is
||| the same kind of deliberate escape hatch as `raw`.
export %inline
unsafeAt : (i : Int) -> At cap w
unsafeAt = MkAt

%foreign "C:offler_f32_new,liboffler"
         "javascript:lambda:(n)=>new Float32Array(n)"
prim__new : Int -> PrimIO AnyPtr

%foreign "C:offler_f32_poke,liboffler"
         "javascript:lambda:(a,i,v)=>{a[i]=v}"
prim__poke : AnyPtr -> Int -> Double -> PrimIO ()

%foreign "C:offler_f32_poke4,liboffler"
         "javascript:lambda:(a,o,x,y,z,w)=>{a[o]=x;a[o+1]=y;a[o+2]=z;a[o+3]=w}"
prim__poke4 : AnyPtr -> Int -> Double -> Double -> Double -> Double -> PrimIO ()

||| Sixteen components in one call. Sixteen separate pokes per matrix was half
||| the orrery's per-frame cost at scale; it is also what makes line overlays
||| affordable to build, at four padded vertices a call.
%foreign "C:offler_f32_poke16,liboffler"
         "javascript:lambda:(a,o,x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)=>{a[o+0]=x0;a[o+1]=x1;a[o+2]=x2;a[o+3]=x3;a[o+4]=x4;a[o+5]=x5;a[o+6]=x6;a[o+7]=x7;a[o+8]=x8;a[o+9]=x9;a[o+10]=x10;a[o+11]=x11;a[o+12]=x12;a[o+13]=x13;a[o+14]=x14;a[o+15]=x15}"
prim__poke16 : AnyPtr -> Int -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> PrimIO ()

%foreign "C:offler_f32_peek,liboffler"
         "javascript:lambda:(a,i)=>a[i]"
prim__peek : AnyPtr -> Int -> PrimIO Double

export
newF32 : (n : Int) -> IO (F32Array n)
newF32 n = (\p => MkF32 p n) <$> primIO (prim__new n)

export
poke : F32Array cap -> At cap 1 -> Double -> IO ()
poke a (MkAt i) v = primIO (prim__poke (raw a) i v)

export
poke4 : F32Array cap -> At cap 4 -> Double -> Double -> Double -> Double -> IO ()
poke4 a (MkAt o) x y z w = primIO (prim__poke4 (raw a) o x y z w)

export
poke16 : F32Array cap -> At cap 16
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double -> IO ()
poke16 a (MkAt o) x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
  primIO (prim__poke16 (raw a) o x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

export
peek : F32Array cap -> At cap 1 -> IO Double
peek a (MkAt i) = primIO (prim__peek (raw a) i)

--------------------------------------------------------------------------------
-- Vertex data

||| An array together with the number of vertices it holds: `count` times
||| `stride` floats, and the array is big enough for them.
|||
||| A backend can neither be told a count the buffer cannot back nor be handed
||| vertices of the wrong stride: different strides are different types.
export
data Verts : (0 stride : Int) -> Type where
  MkVerts : AnyPtr -> (count : Int) -> (floats : Int) -> Verts stride

||| A buffer to fill, and the handle to hand over once it is filled.
||| (The capacity field is `bufCap` rather than `cap` so the projection does
||| not shadow every signature that binds `cap` as an implicit.)
public export
record VertBuf (0 stride : Int) where
  constructor MkVertBuf
  0 bufCap : Int
  arr : F32Array bufCap
  ||| What `createMesh` and `setLines` take. Nothing can be written through it.
  handle : Verts stride

||| Allocate for exactly `count` vertices and pair the array with its count in
||| one step.
|||
||| The pairing is the point. A checking constructor -- take an array and a
||| count, compare, return `Maybe` -- would leave the caller a failure case that
||| cannot happen and no way to say so. Allocating and counting together removes
||| the disagreement instead of detecting it: there is one number, used twice,
||| here.
export
newVerts : {stride : Int} -> (count : Int) -> IO (VertBuf stride)
newVerts count =
  let n = max 0 count
      floats = n * stride
   in do a <- newF32 (max 1 floats)
         pure (MkVertBuf (max 1 floats) a (MkVerts (raw a) n floats))

export %inline
vertsRaw : Verts stride -> AnyPtr
vertsRaw (MkVerts p _ _) = p

||| Vertices, which is what a draw call counts.
export %inline
vertsCount : Verts stride -> Int
vertsCount (MkVerts _ n _) = n

||| Floats, which is what an upload counts. Kept beside the count rather than
||| recomputed by each backend, because recomputing it is where the two would
||| drift apart again.
export %inline
vertsFloats : Verts stride -> Int
vertsFloats (MkVerts _ _ f) = f
