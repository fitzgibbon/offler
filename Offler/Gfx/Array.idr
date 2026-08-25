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
|||
||| The capacity is a `Nat`, not an `Int`, and that is the whole reason the
||| upload loops can prove their writes rather than test them. `So` over a
||| symbolic `Int` never reduces -- not even `So (i <= i)` -- because
||| `prim__lte_Int` is opaque, so a computed offset could only ever be
||| checked at run time, with an unreachable failure branch to answer for.
||| At `Nat` the same bound is an ordinary `LTE`, discharged at multiplicity
||| 0. Offsets stay `Int`: they cross the FFI, and nothing is proved about
||| them once the position exists.
module Offler.Gfx.Array

-- Re-exported: every caller of `here` or `sub` needs the bound discharged
-- by proof search, which is `Data.Nat`'s `LTE` now rather than `So` over
-- `Int`. `Data.So` stays re-exported for `Offler.Gfx.Material`'s `FitsSlot`.
import public Data.Nat
import public Data.So

import Offler.Gfx.Layout

%default total

||| Float widths, bound as names rather than written as literals in the
||| signatures below. With a bare `Nat` literal in `poke16`'s sixteen-argument
||| type the elaborator goes superlinear and never finishes; naming the width
||| costs nothing and it reads better besides.
public export
scalarFloats : Nat
scalarFloats = 1

public export
vec4Floats : Nat
vec4Floats = 4

public export
mat4Floats : Nat
mat4Floats = 16

||| Opaque so a vertex buffer cannot be confused with any other foreign handle,
||| and indexed so its capacity travels with it.
export
data F32Array : (0 cap : Nat) -> Type where
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
data At : (0 cap : Nat) -> (0 w : Nat) -> Type where
  MkAt : Int -> At cap w

||| A literal offset into a known capacity. The bound is discharged by proof
||| search at multiplicity 0, so this costs nothing at all and an offset that
||| does not fit is a compile error.
export %inline
here : (i : Nat) -> {auto 0 ok : LTE (i + w) cap} -> At cap w
here i = MkAt (cast i)

||| A position proved rather than tested, for a *computed* index: element
||| `i` of a run of `n`, each `w` floats wide, in a buffer of exactly
||| `n * w`. This is what an upload loop uses instead of `window`, and it
||| is why those loops no longer have a failure branch that cannot happen.
export %inline
strided : {w : Nat} -> (i : Nat) -> {0 n : Nat} -> (0 ok : LT i n)
       -> At (n * w) w
strided i _ = MkAt (cast i * cast w)

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
window : {w : Nat} -> F32Array cap -> (i : Int) -> Maybe (At cap w)
window (MkF32 _ n) i =
  let wi = the Int (cast w)
   in if i >= 0 && i <= n - wi then Just (MkAt i) else Nothing

||| Step `k` floats into a window, keeping `w'` of it. Arithmetic on a bound
||| already established, not a second check, so this is free too.
export %inline
sub : (k : Nat) -> At cap w -> {auto 0 ok : LTE (k + w') w} -> At cap w'
sub k (MkAt i) = MkAt (i + cast k)

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
newF32 : (n : Nat) -> IO (F32Array n)
newF32 n = let ni = the Int (cast n)
            in (\p => MkF32 p ni) <$> primIO (prim__new ni)

export
poke : F32Array cap -> At cap Offler.Gfx.Array.scalarFloats -> Double -> IO ()
poke a (MkAt i) v = primIO (prim__poke (raw a) i v)

export
poke4 : F32Array cap -> At cap Offler.Gfx.Array.vec4Floats -> Double -> Double -> Double -> Double -> IO ()
poke4 a (MkAt o) x y z w = primIO (prim__poke4 (raw a) o x y z w)

export
poke16 : F32Array cap -> At cap Offler.Gfx.Array.mat4Floats
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double
       -> Double -> Double -> Double -> Double -> IO ()
poke16 a (MkAt o) x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 =
  primIO (prim__poke16 (raw a) o x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)

export
peek : F32Array cap -> At cap Offler.Gfx.Array.scalarFloats -> IO Double
peek a (MkAt i) = primIO (prim__peek (raw a) i)

--------------------------------------------------------------------------------
-- Vertex data

||| An array together with the number of vertices it holds: `count` times
||| the topology's stride in floats, and the array is big enough for them.
|||
||| Indexed by `Topology`, because in bevy's model -- and offler's -- the
||| primitive topology is a property of the mesh, and the vertex layout
||| follows from it. A backend can neither be told a count the buffer cannot
||| back nor be handed line vertices where triangles are expected: different
||| topologies are different types.
export
data Verts : (0 t : Topology) -> Type where
  MkVerts : AnyPtr -> (count : Int) -> (floats : Int) -> Verts t

||| A buffer to fill, and the handle to hand over once it is filled.
||| (The capacity field is `bufCap` rather than `cap` so the projection does
||| not shadow every signature that binds `cap` as an implicit.)
public export
record VertBuf (0 t : Topology) where
  constructor MkVertBuf
  0 bufCap : Nat
  arr : F32Array bufCap
  ||| What `createMesh` takes. Nothing can be written through it.
  handle : Verts t

||| Allocate for exactly `count` vertices of a topology and pair the array
||| with its count in one step.
|||
||| The pairing is the point. A checking constructor -- take an array and a
||| count, compare, return `Maybe` -- would leave the caller a failure case
||| that cannot happen and no way to say so. Allocating and counting together
||| removes the disagreement instead of detecting it: there is one number,
||| used twice, here.
||| Allocate for exactly `count` vertices of a topology and pair the array
||| with its count in one step. The capacity is `count * floatsOfN t`
||| *exactly* -- not `max 1` of it -- because that is the number an upload
||| loop's `strided` bound is stated against. A zero-vertex mesh therefore
||| gets a zero-length array, which nothing writes to.
||| As `newVerts`, but keeping the capacity in the array's type: what the
||| proof-carrying upload loops need, since `VertBuf`'s field is existential
||| and forgets the tie to the count. The `Verts` constructor stays private:
||| allocation and count still happen in one step, here.
export
newVertsAt : {t : Topology} -> (count : Nat)
          -> IO (F32Array (count * floatsOfN t), Verts t)
newVertsAt count = do
  a <- newF32 (count * floatsOfN t)
  pure (a, MkVerts (raw a) (cast count) (cast (count * floatsOfN t)))

export
newVerts : {t : Topology} -> (count : Nat) -> IO (VertBuf t)
newVerts count =
  let floats = count * floatsOfN t
      fi = the Int (cast floats)
   in do a <- newF32 floats
         pure (MkVertBuf floats a (MkVerts (raw a) (cast count) fi))

export %inline
vertsRaw : Verts t -> AnyPtr
vertsRaw (MkVerts p _ _) = p

||| Vertices, which is what a draw call counts.
export %inline
vertsCount : Verts t -> Int
vertsCount (MkVerts _ n _) = n

||| Floats, which is what an upload counts. Kept beside the count rather than
||| recomputed by each backend, because recomputing it is where the two would
||| drift apart again.
export %inline
vertsFloats : Verts t -> Int
vertsFloats (MkVerts _ _ f) = f

--------------------------------------------------------------------------------
-- Index data

%foreign "C:offler_u32_new,liboffler"
         "javascript:lambda:(n)=>new Uint32Array(n)"
prim__u32new : Int -> PrimIO AnyPtr

%foreign "C:offler_u32_poke,liboffler"
         "javascript:lambda:(a,i,v)=>{a[i]=v}"
prim__u32poke : AnyPtr -> Int -> Int -> PrimIO ()

||| A mesh's index list: `count` u32 indices, allocated and counted in one
||| step like `Verts`. Writes are bounds-tested with a bare comparison --
||| indices are built once, at mesh construction, off every hot path.
export
data Indices : Type where
  MkIndices : AnyPtr -> (len : Int) -> Indices

export
newIndices : (count : Int) -> IO Indices
newIndices count = do
  let n = max 1 count
  p <- primIO (prim__u32new n)
  pure (MkIndices p (max 0 count))

export
pokeIndex : Indices -> (i : Int) -> (v : Int) -> IO ()
pokeIndex (MkIndices p n) i v =
  when (i >= 0 && i < n) (primIO (prim__u32poke p i v))

export %inline
indicesRaw : Indices -> AnyPtr
indicesRaw (MkIndices p _) = p

export %inline
indicesCount : Indices -> Int
indicesCount (MkIndices _ n) = n
