||| The ECS's foreign storage primitives: an `Int` column for entity
||| indices, generations and sparse-set slots, a bulk copy for query
||| snapshots, and an in-place float move for packed swap-removes.
|||
||| This is the only module in `Offler.Ecs` that names a backend. Every
||| primitive is one declaration with one specifier per backend, the pattern
||| `Offler.Gfx.Array` uses, so a further backend adds a specifier here and
||| nothing above changes.
|||
||| Reads and writes are unchecked: positions come from the stores'
||| invariants (an entity index is below the world's capacity because
||| `Entity` cannot be forged, a dense index is below the live count), never
||| from callers.
module Offler.Ecs.Column

import Offler.Gfx.Array

%default total

%foreign "C:offler_i32_new,liboffler"
         "javascript:lambda:(n)=>new Int32Array(Math.max(n,1))"
prim__i32new : Int -> PrimIO AnyPtr

%foreign "scheme,chez:(lambda (a i) (foreign-ref 'integer-32 a (fx* i 4)))"
         "C:offler_i32_peek,liboffler"
         "javascript:lambda:(a,i)=>a[i]"
prim__i32peek : AnyPtr -> Int -> PrimIO Int

%foreign "scheme,chez:(lambda (a i v) (foreign-set! 'integer-32 a (fx* i 4) v))"
         "C:offler_i32_poke,liboffler"
         "javascript:lambda:(a,i,v)=>{a[i]=v}"
prim__i32poke : AnyPtr -> Int -> Int -> PrimIO ()

%foreign "C:offler_i32_copy,liboffler"
         "javascript:lambda:(d,s,n)=>{d.set(s.subarray(0,n))}"
prim__i32copy : AnyPtr -> AnyPtr -> Int -> PrimIO ()

%foreign "C:offler_f32_move,liboffler"
         "javascript:lambda:(a,d,s,n)=>{a.copyWithin(d,s,s+n)}"
prim__f32move : AnyPtr -> Int -> Int -> Int -> PrimIO ()

||| A zero-filled column of 32-bit integers.
export
data I32Array : Type where
  MkI32 : AnyPtr -> I32Array

export
newI32 : (n : Int) -> IO I32Array
newI32 n = MkI32 <$> primIO (prim__i32new n)

export %inline
peekI : I32Array -> Int -> PrimIO Int
peekI (MkI32 a) = prim__i32peek a

export %inline
pokeI : I32Array -> Int -> Int -> PrimIO ()
pokeI (MkI32 a) = prim__i32poke a

export %inline
readI : I32Array -> Int -> IO Int
readI a i = fromPrim (peekI a i)

export %inline
writeI : I32Array -> Int -> Int -> IO ()
writeI a i v = fromPrim (pokeI a i v)

||| Copy the first `n` entries of `src` over the start of `dst`: one
||| foreign call however many entities a query is about to visit.
export
copyI32 : (dst : I32Array) -> (src : I32Array) -> (n : Int) -> IO ()
copyI32 (MkI32 d) (MkI32 s) n = primIO (prim__i32copy d s n)

||| Move `n` floats from `src` to `dst` within one array, in one call: a
||| packed component's swap-remove, whatever its width.
export
moveF32 : F32Array cap -> (dst : Int) -> (src : Int) -> (n : Int) -> IO ()
moveF32 a d s n = primIO (prim__f32move (raw a) d s n)

||| Comparisons on `Int` written against the primitive. `x /= 0` goes
||| through `Eq Int` with the literal converted from a BigInt on every call
||| on the JavaScript backend; these inline to a bare `===`.
export %inline
eqI : Int -> Int -> Bool
eqI a b = intToBool (prim__eq_Int a b)

export %inline
nonZero : Int -> Bool
nonZero a = not (eqI a 0)

||| Evaluate a value here, once, and bind it. The ECS builds its
||| per-entity closures and partial applications through this: a pure
||| binding used once is inlined into the closure that uses it, and is then
||| rebuilt -- or a store re-matched, or a dictionary method re-applied --
||| on every entity. `%noinline` keeps the call, and a strict argument is
||| evaluated before it.
export %noinline
once : a -> IO a
once x = pure x
