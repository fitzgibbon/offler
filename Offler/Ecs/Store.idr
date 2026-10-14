||| Component storage: one sparse set per component type, its values in
||| one of four representations chosen by the component itself.
|||
||| A sparse set is two `Int` columns: `slot` maps an entity index to its
||| position in the dense run (plus one; zero means absent), and `dense`
||| maps back. Insertion appends, removal swaps the last element into the
||| hole, so the run stays contiguous -- which is what lets a packed
||| store's float column be handed to the GPU as it stands.
|||
||| The representations:
|||
||| * `Packed` -- a `Columnar` component, its fields in a foreign `F32Array`
|||   at a fixed stride. No Idris value per entity at rest: Chez boxes
|||   every `Double` in a record, a column boxes nothing.
||| * `Boxed` -- any Idris value, in a mutable array. For what is not
|||   numeric: drawables, entity references, strings.
||| * `Marker` -- membership only, for zero-sized tags.
||| * `Global` -- one value for the whole world: a resource.
module Offler.Ecs.Store

import Data.IORef
import Data.IOArray.Prims
import public Offler.Gfx.Array
import Offler.Ecs.Column

%default total

||| A component whose value is a fixed run of floats. `width` is the run's
||| length; `storeCol`/`loadCol` write and read one value at a position the
||| store has already bounded, so instances address their fields with
||| `field`, whose bounds are proved at compile time.
public export
interface Columnar (0 c : Type) where
  width : Nat
  storeCol : F32Array cap -> At cap width -> c -> IO ()
  loadCol : F32Array cap -> At cap width -> IO c

||| How a component is stored. Chosen once per component type, by its
||| `Component` instance -- APECS's `type Storage c`.
public export
data Kind : Type -> Type where
  Packed : Columnar c => Kind c
  Boxed : Kind c
  ||| A tag: the type's one value, stored once rather than per entity.
  Marker : c -> Kind c
  ||| One value for the whole world, read by every entity that asks: bevy's
  ||| `Res`, APECS's `Global`. Starts at the value given.
  Global : c -> Kind c

||| A sparse set, plus the change ticks of each dense slot: when its value
||| was last written, and when the component was added. Ticks move with
||| the value on a swap-remove.
record Sparse where
  constructor MkSparse
  slot : I32Array
  dense : I32Array
  added : I32Array
  changed : I32Array
  count : IORef Int

newSparse : Int -> IO Sparse
newSparse cap =
  MkSparse <$> newI32 cap <*> newI32 cap <*> newI32 cap <*> newI32 cap <*> newIORef 0

||| One component type's storage. The constructors are private, so a store
||| is made by `newStore` from its component's `Kind` and no other way.
export
data Store : Type -> Type where
  ||| `put` and `get` are the component's `storeCol`/`loadCol`, closed
  ||| over the column once here rather than looked up per entity.
  MkPacked : {0 k : Nat} -> (w : Int) -> Sparse -> F32Array k
          -> (put : Int -> c -> PrimIO ()) -> (get : Int -> PrimIO c) -> Store c
  MkBoxed : Sparse -> ArrayData (Maybe c) -> Store c
  MkMarker : Sparse -> c -> Store c
  MkGlobal : IORef c -> (changedAt : IORef Int) -> Store c

export
newStore : Kind c -> (cap : Int) -> IO (Store c)
newStore Packed cap = do
  let w = the Int (cast (width {c}))
  s <- newSparse cap
  a <- newF32 (cast (max 1 cap * w))
  sc <- once (storeCol a)
  lc <- once (loadCol a)
  pure (MkPacked w s a (\off, v => toPrim (sc (unsafeAt off) v))
                       (\off => toPrim (lc (unsafeAt off))))
newStore Boxed cap = do
  s <- newSparse cap
  a <- fromPrim (prim__newArray (max 1 cap) Nothing)
  pure (MkBoxed s a)
newStore (Marker v) cap = (\s => MkMarker s v) <$> newSparse cap
newStore (Global v) _ = MkGlobal <$> newIORef v <*> newIORef 0

||| Whether a query can iterate this store's members. A global has no
||| members of its own: it is read by whichever entities the rest of the
||| query finds.
export
drives : Store c -> Bool
drives (MkGlobal _ _) = False
drives _ = True

||| Entities holding this component; one for a global.
export
size : Store c -> IO Int
size (MkPacked _ s _ _ _) = readIORef s.count
size (MkBoxed s _) = readIORef s.count
size (MkMarker s _) = readIORef s.count
size (MkGlobal _ _) = pure 1

||| Copy the dense entity indices into `dst`, returning how many: the
||| snapshot a query iterates, so the store may change underneath it.
export
snapshot : Store c -> I32Array -> IO Int
snapshot st dst = case st of
  MkPacked _ s _ _ _ => go s
  MkBoxed s _ => go s
  MkMarker s _ => go s
  MkGlobal _ _ => pure 0
  where
    go : Sparse -> IO Int
    go s = do
      n <- readIORef s.count
      copyI32 dst s.dense n
      pure n

inSparse : Sparse -> Int -> PrimIO Bool
inSparse s i w = case peekI s.slot i w of
  MkIORes d w' => MkIORes (nonZero d) w'

||| Every index, or none.
constP : Bool -> Int -> PrimIO Bool
constP b _ w = MkIORes b w

||| A membership test for this store, built once. The index is a live
||| entity's, so below the capacity the columns were made with.
export
member : Store c -> IO (Int -> PrimIO Bool)
member (MkPacked _ s _ _ _) = once (inSparse s)
member (MkBoxed s _) = once (inSparse s)
member (MkMarker s _) = once (inSparse s)
member (MkGlobal _ _) = once (constP True)

||| A tick test for this store: members whose tick column holds a value
||| newer than `since`. `col` picks the column -- added or changed.
tickTest : (Sparse -> I32Array) -> Sparse -> (since : Int) -> Int -> PrimIO Bool
tickTest col s since i w = case peekI s.slot i w of
  MkIORes d w1 =>
    if nonZero d
      then case peekI (col s) (d - 1) w1 of
        MkIORes t w2 => MkIORes (t > since) w2
      else MkIORes False w1

||| Members whose value was written after tick `since`: bevy's
||| `Changed<T>`. A global matches everything when it was written since.
export
changedSince : Store c -> (since : Int) -> IO (Int -> PrimIO Bool)
changedSince (MkPacked _ s _ _ _) n = once (tickTest (.changed) s n)
changedSince (MkBoxed s _) n = once (tickTest (.changed) s n)
changedSince (MkMarker s _) n = once (tickTest (.changed) s n)
changedSince (MkGlobal _ at) n = do
  t <- readIORef at
  once (constP (t > n))

||| Members that gained the component after tick `since`: bevy's
||| `Added<T>`. A global exists from the start, so is never added.
export
addedSince : Store c -> (since : Int) -> IO (Int -> PrimIO Bool)
addedSince (MkPacked _ s _ _ _) n = once (tickTest (.added) s n)
addedSince (MkBoxed s _) n = once (tickTest (.added) s n)
addedSince (MkMarker s _) n = once (tickTest (.added) s n)
addedSince (MkGlobal _ _) _ = once (constP False)

||| A reader for this store, matched on its representation once rather
||| than per entity. Only called for an index the membership test
||| accepted; the crash branch is the store invariant failing, which no
||| caller can arrange.
|||
||| These are `IO` so the closure they return is built once, at the bind:
||| a pure function whose clauses are lambdas gets eta-expanded, and then
||| matches and allocates on every call.
export
reader : Store c -> IO (Int -> PrimIO c)
reader (MkPacked w s _ _ get) = once $ \i, wd =>
  case peekI s.slot i wd of
    MkIORes d wd' => get ((d - 1) * w) wd'
reader (MkBoxed s a) = once $ \i, wd =>
  case peekI s.slot i wd of
    MkIORes d wd' => case prim__arrayGet a (d - 1) wd' of
      MkIORes (Just v) wd'' => MkIORes v wd''
      MkIORes Nothing wd'' => MkIORes (assert_total (idris_crash "Offler.Ecs: boxed slot empty under a live entry")) wd''
reader (MkMarker _ v) = once $ \_, wd => MkIORes v wd
reader (MkGlobal ref _) = once $ \_ => toPrim (readIORef ref)

||| Append the entity at the end of the dense run, stamped as added and
||| changed at `tick`, returning its position.
claim : Sparse -> (tick : Int) -> Int -> IO Int
claim s tick i = do
  n <- readIORef s.count
  writeI s.dense n i
  writeI s.slot i (n + 1)
  writeI s.added n tick
  writeIORef s.count (n + 1)
  pure n

||| The dense position the entity's value goes to, claimed if absent, with
||| its changed tick stamped.
place : Sparse -> (tick : Int) -> Int -> PrimIO Int
place s tick i wd = case peekI s.slot i wd of
  MkIORes d wd1 =>
    if nonZero d
      then case pokeI s.changed (d - 1) tick wd1 of
        MkIORes _ wd2 => MkIORes (d - 1) wd2
      else case toPrim (claim s tick i) wd1 of
        MkIORes pos wd2 => case pokeI s.changed pos tick wd2 of
          MkIORes _ wd3 => MkIORes pos wd3

||| A writer stamping `tick`: overwrite in place when present, append when
||| not.
export
writer : Store c -> (tick : Int) -> IO (Int -> c -> PrimIO ())
writer (MkPacked w s _ put _) tick = once $ \i, v, wd =>
  case place s tick i wd of
    MkIORes pos wd1 => put (pos * w) v wd1
writer (MkBoxed s a) tick = once $ \i, v, wd =>
  case place s tick i wd of
    MkIORes pos wd1 => prim__arraySet a pos (Just v) wd1
writer (MkMarker s _) tick = once $ \i, _, wd =>
  case place s tick i wd of
    MkIORes _ wd1 => MkIORes () wd1
writer (MkGlobal ref at) tick = once $ \_, v => toPrim $ do
  writeIORef ref v
  writeIORef at tick

||| Remove the entity's component, if it has one: the last element moves
||| into the hole, ticks and all, so the run stays contiguous. A boxed slot
||| vacated at the end is cleared, so the removed value is released now
||| rather than whenever the slot is next reused. A global cannot be
||| removed.
export
remove : Store c -> Int -> IO ()
remove st i = case st of
  MkPacked w s a _ _ => go s (\dst, src => moveF32 a (dst * w) (src * w) w) (\_ => pure ())
  MkBoxed s a => go s (\dst, src => fromPrim (prim__arrayGet a src) >>= \v => fromPrim (prim__arraySet a dst v))
                      (\at => fromPrim (prim__arraySet a at Nothing))
  MkMarker s _ => go s (\_, _ => pure ()) (\_ => pure ())
  MkGlobal _ _ => pure ()
  where
    go : Sparse -> (Int -> Int -> IO ()) -> (Int -> IO ()) -> IO ()
    go s moveValue clearValue = do
      d <- readI s.slot i
      when (nonZero d) $ do
        let hole = d - 1
        n <- readIORef s.count
        let lst = n - 1
        when (not (eqI hole lst)) $ do
          e <- readI s.dense lst
          writeI s.dense hole e
          writeI s.slot e (hole + 1)
          readI s.added lst >>= writeI s.added hole
          readI s.changed lst >>= writeI s.changed hole
          moveValue hole lst
        clearValue lst
        writeI s.slot i 0
        writeIORef s.count lst

||| A packed store's float column and live count, for consumers that hand
||| the column on whole -- `Offler.Ecs.Render` uploads an instance batch
||| this way. `Nothing` for the other representations.
export
withColumn : Store c -> ({0 k : Nat} -> F32Array k -> (count : Int) -> IO a) -> IO (Maybe a)
withColumn (MkPacked _ s a _ _) f = Just <$> (readIORef s.count >>= f a)
withColumn _ _ = pure Nothing
