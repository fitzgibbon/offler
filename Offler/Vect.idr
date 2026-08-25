||| Tail-recursive `Vect` operations, and the `%transform`s that make the
||| ordinary spellings use them.
|||
||| `Data.Vect`'s own `map`, `++`, `concat`, `toList`, `length` and `foldr`
||| are written as plain structural recursion: they cons, so they spend a
||| stack frame per element. On the JavaScript backend that overflows V8
||| between 5 000 and 8 000 elements -- well inside the sizes offler's own
||| meshes reach (a 64x64 torus is 8 192 triangles, an icosphere at
||| subdivision 5 is 20 480). `Prelude`'s *list* versions do not have this
||| problem, because the Prelude ships `%transform`s rewriting them into
||| accumulator-passing form; `Data.Vect` ships none. This module is that
||| missing half, written the same way and for the same reason
||| `Offler.Math.range` is hand-rolled.
|||
||| Each operation is an accumulator loop plus a `reverse` (which is already
||| tail recursive in base). The index arithmetic that an accumulator forces
||| -- `m + S k` where the goal wants `S m + k` -- is discharged by
||| `plusSuccRightSucc` and friends, at multiplicity 0, so none of it exists
||| at run time.
|||
||| **The transforms are not inherited.** A `%transform` applies in the
||| module that declares it, and its right-hand side must be local to that
||| module -- importing this one does *not* redirect your `map`. Modules
||| doing bulk `Vect` work therefore repeat the four-line `alias +
||| %transform` block; `Offler.Mesh` and `Offler.Gizmos` both do. Application
||| code mapping over a large `Vect` this library returned should either call
||| `mapV` here directly or paste the same block.
module Offler.Vect

import public Data.Vect

import Data.Fin
import Data.Nat

%default total

--------------------------------------------------------------------------------
-- Building

||| `Vect n` from an index function, ascending: `f 0, f 1, .. f (n-1)`.
|||
||| The counterpart of `Offler.Math.range` for vectors, and tail recursive
||| for the same reason: this is what every parametric mesh builder counts
||| with, and those reach tens of thousands of elements.
public export
tabulate : (n : Nat) -> (Nat -> a) -> Vect n a
tabulate n f = reverse (go n 0 [])
  where
    go : (k : Nat) -> (i : Nat) -> Vect m a -> Vect (m + k) a
    go {m} Z i acc = rewrite plusZeroRightNeutral m in acc
    go {m} (S j) i acc =
      rewrite sym (plusSuccRightSucc m j) in go j (S i) (f i :: acc)

||| `Vect n` from a *bounded* index function: the index is a `Fin n`, so a
||| tabulated element cannot be asked for out of range, and the `Fin`s it
||| hands out are exactly the legal indices of the result.
public export
tabulateFin : (n : Nat) -> (Fin n -> a) -> Vect n a
tabulateFin n f = reverse (go n 0 Refl [])
  where
    -- `i + k = n` is the loop invariant: `i` consumed, `k` left. While `k`
    -- is a successor it witnesses `LT i n`, which is what mints the `Fin n`.
    step : (i, j : Nat) -> (0 inv : i + S j = n) -> Fin n
    step i j inv =
      natToFinLT i {prf = rewrite sym (trans (plusSuccRightSucc i j) inv)
                            in LTESucc (lteAddRight i)}

    go : (k : Nat) -> (i : Nat) -> (0 inv : i + k = n) -> Vect m a -> Vect (m + k) a
    go {m} Z i inv acc = rewrite plusZeroRightNeutral m in acc
    go {m} (S j) i inv acc =
      rewrite sym (plusSuccRightSucc m j) in
      go j (S i) (trans (plusSuccRightSucc i j) inv) (f (step i j inv) :: acc)

--------------------------------------------------------------------------------
-- Tail-recursive replacements

public export
mapV : (a -> b) -> Vect n a -> Vect n b
mapV f v = reverse (go v [])
  where
    go : Vect k a -> Vect m b -> Vect (m + k) b
    go {m} [] acc = rewrite plusZeroRightNeutral m in acc
    go {m} ((::) {len} x xs) acc =
      rewrite sym (plusSuccRightSucc m len) in go xs (f x :: acc)

public export
appendV : Vect m a -> Vect n a -> Vect (m + n) a
appendV xs ys = go (reverse xs) ys
  where
    go : Vect j a -> Vect k a -> Vect (j + k) a
    go [] acc = acc
    go {k} ((::) {len} x xs) acc =
      rewrite plusSuccRightSucc len k in go xs (x :: acc)

public export
lengthV : Vect n a -> Nat
lengthV v = go v 0
  where
    go : Vect k a -> Nat -> Nat
    go [] acc = acc
    go (_ :: xs) acc = go xs (S acc)

public export
toListV : Vect n a -> List a
toListV v = go (reverse v) []
  where
    go : Vect k a -> List a -> List a
    go [] acc = acc
    go (x :: xs) acc = go xs (x :: acc)

public export
foldrV : (a -> b -> b) -> b -> Vect n a -> b
foldrV f z v = go (reverse v) z
  where
    go : Vect k a -> b -> b
    go [] acc = acc
    go (x :: xs) acc = go xs (f x acc)

public export
concatV : {0 a : Type} -> {0 n : Nat} -> Vect m (Vect n a) -> Vect (m * n) a
concatV {m} v = rewrite sym (plusZeroRightNeutral (m * n)) in go (reverse v) []
  where
    go : Vect j (Vect n a) -> Vect k a -> Vect (j * n + k) a
    go [] acc = acc
    go {k} ((::) {len} g gs) acc =
      rewrite sym (plusCommutative (len * n) n) in
      rewrite sym (plusAssociative (len * n) n k) in
      go gs (appendV g acc)

||| `n` groups of `k`, flattened: what every parametric mesh builder does --
||| two triangles per grid cell, four per cylinder segment, and so on.
public export
tabulateFlat : (n : Nat) -> {k : Nat} -> (Nat -> Vect k a) -> Vect (n * k) a
tabulateFlat n f = concatV (tabulate n f)

||| `tabulateFin` flattened: `n` bounded groups of `k`.
public export
tabulateFinFlat : (n : Nat) -> {k : Nat} -> (Fin n -> Vect k a) -> Vect (n * k) a
tabulateFinFlat n f = concatV (tabulateFin n f)

--------------------------------------------------------------------------------
-- Bounded grid indexing

||| Multiplication is monotone in its left factor. `base` ships neither this
||| nor its mirror, which is what makes strided-index bounds awkward to prove
||| by hand -- so it is proved once, here.
public export
multLteLeft : (c : Nat) -> {k, n : Nat} -> LTE k n -> LTE (c * k) (c * n)
multLteLeft Z _ = LTEZero
multLteLeft (S c) p = plusLteMonotone p (multLteLeft c p)

public export
multLteRight : (r : Nat) -> {k, n : Nat} -> LTE k n -> LTE (k * r) (n * r)
multLteRight r prf =
  rewrite multCommutative k r in
  rewrite multCommutative n r in
  multLteLeft r prf

||| A `Fin`'s value is below its bound. base 0.8.0 ships no such lemma.
public export
finBound : (i : Fin m) -> LT (finToNat i) m
finBound FZ = LTESucc LTEZero
finBound (FS k) = LTESucc (finBound k)

||| The row-major index of a grid cell: `(i, j)` in an `m x n` grid is
||| `i * n + j`, and the result is a `Fin (m * n)` -- so a grid index cannot
||| name a vertex that is not there.
|||
||| This is the bound that `indexedGrid` used to leave to arithmetic. A
||| slipped `+ 1` produced an out-of-range `Int`, which `pokeIndex` answered
||| by dropping the write, and the mesh came out wrong with nothing reporting
||| it anywhere.
public export
pairIndex : {m, n : Nat} -> Fin m -> Fin n -> Fin (m * n)
pairIndex i j = natToFinLT (finToNat i * n + finToNat j) {prf = bound}
  where
    bound : LT (finToNat i * n + finToNat j) (m * n)
    bound =
      let jLt : LT (finToNat j) n
          jLt = finBound j
          iLt : LT (finToNat i) m
          iLt = finBound i
          -- i*n + j < i*n + n = n + i*n = S i * n
          step1 : LT (finToNat i * n + finToNat j) (finToNat i * n + n)
          step1 = rewrite plusSuccRightSucc (finToNat i * n) (finToNat j) in
                    plusLteMonotoneLeft (finToNat i * n) (S (finToNat j)) n jLt
          step2 : LTE (finToNat i * n + n) (S (finToNat i) * n)
          step2 = rewrite plusCommutative (finToNat i * n) n in reflexive
          -- S i * n <= m * n
          step3 : LTE (S (finToNat i) * n) (m * n)
          step3 = multLteRight n iLt
       in transitive step1 (transitive step2 step3)

||| A strided write stays inside a buffer sized for the whole run: element
||| `i` of `n`, each `w` wide, ends at or before `n * w`.
|||
||| This is the bound every upload loop needs and could not have while the
||| buffer's capacity was an `Int`: `So` on a symbolic `Int` does not reduce,
||| so the write had to be *tested* at run time and the failing branch --
||| which could not be taken -- silently dropped the write. At `Nat` it is
||| two lines.
public export
strideBound : (w : Nat) -> {i, n : Nat} -> LT i n -> LTE (i * w + w) (n * w)
strideBound w prf = rewrite plusCommutative (i * w) w in multLteRight w prf

||| The loop-invariant bound every counted writer uses: if `done + S j = n`
||| then `done < n`. This is what lets a loop that walks a `Vect` while
||| counting up prove, at each step, that its index is in range -- the same
||| derivation `tabulateFin` makes for its `Fin`s.
public export
0 countLT : (d : Nat) -> {0 j, n : Nat} -> (0 inv : d + S j = n) -> LT d n
countLT d inv = rewrite sym (trans (plusSuccRightSucc d j) inv)
                 in LTESucc (lteAddRight d)
