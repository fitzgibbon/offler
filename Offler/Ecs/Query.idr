||| Systems over components: `cmap`, `cmapM`, `cfor` and `cfold`, APECS's
||| combinators, over queries written as tuple types.
|||
||| A query type is a component, `Entity`, `Maybe c` (optional), `With c`
||| / `Without c` (filters), `Changed c` / `Added c` (change detection,
||| relative to the running system's last run), or a pair of queries -- so
||| `(Pos, Vel)`, `(Entity, Life, Without Frozen)`, `(Pos, Changed Pos)`. A
||| resource in a query reads the one world-wide value for every entity. A write type is a component, `Maybe c`
||| (`Nothing` removes), `Without c` (removes), `()`, or a pair. Both are
||| resolved by interface search against the world's component types, so a
||| query naming a component the world lacks does not compile.
|||
||| Iteration walks a *snapshot* of the smallest required store's entities,
||| re-checking each entity's membership as it goes. Everything is
||| therefore safe inside a system: inserting, removing, spawning and
||| despawning -- entities spawned mid-query are simply not visited, and
||| ones removed mid-query are skipped. There is no command buffer to
||| flush, and no ordering rule for the caller to remember.
|||
||| The loops are `PrimIO` with the world token bound on the left, so the
||| JavaScript backend sees a saturated self tail call and trampolines it;
||| `Checks/EcsCheck.idr` runs one at full capacity and `make check` greps
||| for the trampoline.
module Offler.Ecs.Query

import Data.IORef
import Offler.Ecs.Column
import Offler.Ecs.Entity
import Offler.Ecs.Store
import Offler.Ecs.World

%default total

||| Only entities holding `c`; reads as nothing.
public export
data With : Type -> Type where
  MkWith : With c

||| Only entities *not* holding `c`. As a write, removes `c`.
public export
data Without : Type -> Type where
  MkWithout : Without c

||| Only entities whose `c` was written since the running system last ran:
||| bevy's `Changed<T>`. Every write counts, including an equal value.
public export
data Changed : Type -> Type where
  MkChanged : Changed c

||| Only entities that gained `c` since the running system last ran:
||| bevy's `Added<T>`.
public export
data Added : Type -> Type where
  MkAdded : Added c

||| A store an entity is required to be in. The smallest one present in a
||| query drives its iteration.
export
data Driver : Type where
  MkDriver : Store c -> Driver

||| What a query compiles to, once per system run: the stores that drive
||| it, a membership test, and a reader. All three are closures matched on
||| store representation up front, so the per-entity cost is the calls
||| themselves.
export
record Plan q where
  constructor MkPlan
  drivers : List Driver
  matches : Int -> PrimIO Bool
  fetch : Int -> PrimIO q

public export
interface Query (0 cs : TypeTree) (0 q : Type) where
  plan : World cs -> IO (Plan q)

||| A write plan: what `write` returns, once per system run. Two
||| constructors on purpose: a single-field wrapper is erased as a newtype,
||| and `NoWrite` lets a `()` write skip the call.
export
data Writer : Type -> Type where
  MkWriter : (Int -> r -> PrimIO ()) -> Writer r
  NoWrite : Writer r

||| Run a write plan.
putW : Writer r -> Int -> r -> PrimIO ()
putW (MkWriter f) = f
putW NoWrite = \_, _, wd => MkIORes () wd

public export
interface Write (0 cs : TypeTree) (0 r : Type) where
  write : World cs -> IO (Writer r)

always : Int -> PrimIO Bool
always _ w = MkIORes True w

both : (Int -> PrimIO Bool) -> (Int -> PrimIO Bool) -> Int -> PrimIO Bool
both f g i w = case f i w of
  MkIORes True w' => g i w'
  MkIORes False w' => MkIORes False w'

||| The store as a driver, if it can drive.
driverOf : Store c -> List Driver
driverOf st = if drives st then [MkDriver st] else []

export
(Component c, Has c cs) => Query cs c where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- member st
    rd <- reader st
    pure (MkPlan (driverOf st) m rd)

export
Query cs Entity where
  plan w = let es = entitiesOf w
            in pure (MkPlan [] (aliveAt es) (\i => toPrim (entityOf es i)))

export
(Component c, Has c cs) => Query cs (Maybe c) where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- member st
    rd <- reader st
    pure $ MkPlan [] always $ \i, wd => case m i wd of
      MkIORes True wd' => case rd i wd' of
        MkIORes v wd'' => MkIORes (Just v) wd''
      MkIORes False wd' => MkIORes Nothing wd'

export
(Component c, Has c cs) => Query cs (With c) where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- member st
    pure (MkPlan (driverOf st) m (\_, wd => MkIORes MkWith wd))

export
(Component c, Has c cs) => Query cs (Without c) where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- member st
    pure $ MkPlan [] (\i, wd => case m i wd of
                        MkIORes b wd' => MkIORes (not b) wd')
                     (\_, wd => MkIORes MkWithout wd)

export
(Component c, Has c cs) => Query cs (Changed c) where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- seenTick w >>= changedSince st
    pure (MkPlan (driverOf st) m (\_, wd => MkIORes MkChanged wd))

export
(Component c, Has c cs) => Query cs (Added c) where
  plan w = do
    st <- storeOf w (the (Has c cs) %search)
    m <- seenTick w >>= addedSince st
    pure (MkPlan (driverOf st) m (\_, wd => MkIORes MkAdded wd))

export
(Query cs a, Query cs b) => Query cs (a, b) where
  plan w = do
    pa <- plan {q = a} w
    pb <- plan {q = b} w
    let fa = pa.fetch
        fb = pb.fetch
    pure $ MkPlan (pa.drivers ++ pb.drivers) (both pa.matches pb.matches) $ \i, wd =>
      case fa i wd of
        MkIORes x wd' => case fb i wd' of
          MkIORes y wd'' => MkIORes (x, y) wd''

export
(Component c, Has c cs) => Write cs c where
  write w = do
    st <- storeOf w (the (Has c cs) %search)
    MkWriter <$> (currentTick w >>= writer st)

export
Write cs () where
  write _ = pure NoWrite

export
(Component c, Has c cs) => Write cs (Maybe c) where
  write w = do
    st <- storeOf w (the (Has c cs) %search)
    wr <- currentTick w >>= writer st
    pure $ MkWriter $ \i, mv => case mv of
      Just v => wr i v
      Nothing => toPrim (remove st i)

export
(Component c, Has c cs) => Write cs (Without c) where
  write w = do
    st <- storeOf w (the (Has c cs) %search)
    pure (MkWriter (\i, _ => toPrim (remove st i)))

export
(Write cs a, Write cs b) => Write cs (a, b) where
  write w = do
    wa <- write {r = a} w
    wb <- write {r = b} w
    pure $ case (wa, wb) of
      (NoWrite, NoWrite) => NoWrite
      (MkWriter fa, NoWrite) => MkWriter (\i, (x, _) => fa i x)
      (NoWrite, MkWriter fb) => MkWriter (\i, (_, y) => fb i y)
      (MkWriter fa, MkWriter fb) => MkWriter $ \i, (x, y), wd =>
        case fa i x wd of
          MkIORes _ wd' => fb i y wd'

--------------------------------------------------------------------------------
-- Iteration

||| One snapshot entry: fold `step` over it if it matches. Kept out of the
||| loop so each iteration is a single trampoline state on the JavaScript
||| backend rather than one per nested `case`.
visitId : I32Array -> (Int -> PrimIO Bool) -> (acc -> Int -> PrimIO acc)
       -> acc -> Int -> PrimIO acc
visitId ids m step acc i w =
  case peekI ids i w of
    MkIORes e w1 => case m e w1 of
      MkIORes True w2 => step acc e w2
      MkIORes False w2 => MkIORes acc w2

||| Fold over the matching entities among a snapshot's first `n`.
covering
foldIds : I32Array -> (Int -> PrimIO Bool) -> (acc -> Int -> PrimIO acc)
       -> acc -> (i : Int) -> (n : Int) -> PrimIO acc
foldIds ids m step acc i n w =
  if i >= n
    then MkIORes acc w
    else case visitId ids m step acc i w of
      MkIORes acc' w' => foldIds ids m step acc' (i + 1) n w'

visitLive : Entities -> (Int -> PrimIO Bool) -> (acc -> Int -> PrimIO acc)
         -> acc -> Int -> PrimIO acc
visitLive es m step acc i w =
  case aliveAt es i w of
    MkIORes False w1 => MkIORes acc w1
    MkIORes True w1 => case m i w1 of
      MkIORes True w2 => step acc i w2
      MkIORes False w2 => MkIORes acc w2

||| Fold over every live entity index below `n` that matches: the path for
||| a query that requires no store, such as `(Entity, Maybe Name)`.
covering
foldLive : Entities -> (Int -> PrimIO Bool) -> (acc -> Int -> PrimIO acc)
        -> acc -> (i : Int) -> (n : Int) -> PrimIO acc
foldLive es m step acc i n w =
  if i >= n
    then MkIORes acc w
    else case visitLive es m step acc i w of
      MkIORes acc' w' => foldLive es m step acc' (i + 1) n w'

smallest : List Driver -> IO (Maybe Driver)
smallest [] = pure Nothing
smallest (d@(MkDriver st) :: ds) = do
  n <- size st
  Just <$> go d n ds
  where
    go : Driver -> Int -> List Driver -> IO Driver
    go best _ [] = pure best
    go best bn (d'@(MkDriver st') :: rest) = do
      n' <- size st'
      if n' < bn then go d' n' rest else go best bn rest

||| The one iteration every combinator below is: fold `step` over the
||| entity indices the plan matches.
covering
run : World cs -> Plan q -> (acc -> Int -> PrimIO acc) -> acc -> IO acc
run w p step acc = do
  drv <- smallest p.drivers
  case drv of
    Nothing => do
      let es = entitiesOf w
      n <- highWater es
      fromPrim (foldLive es p.matches step acc 0 n)
    Just (MkDriver st) =>
      withScratch w $ \ids => do
        n <- snapshot st ids
        fromPrim (foldIds ids p.matches step acc 0 n)

||| Map every matching entity's query to a write: APECS's `cmap`, bevy's
||| `Query::iter_mut`. Writing a component the entity lacks inserts it.
export covering
cmap : {0 q, r : Type} -> Query cs q => Write cs r
    => World cs -> (q -> r) -> IO ()
cmap w f = do
  p <- plan {q} w
  wr <- write {r} w
  let fetch = p.fetch
  case wr of
    NoWrite => pure ()
    MkWriter put =>
      run w p (\_, i, wd => case fetch i wd of
                 MkIORes v wd' => put i (f v) wd') ()

||| As `cmap`, with effects: the system may read other entities, spawn,
||| despawn, play a sound. If it despawns the entity it was given, the
||| write is dropped -- checked by generation, so a slot recycled within
||| the same call is not written either.
export covering
cmapM : {0 q, r : Type} -> Query cs q => Write cs r
     => World cs -> (q -> IO r) -> IO ()
cmapM w f = do
  p <- plan {q} w
  wr <- write {r} w
  let es = entitiesOf w
      fetch = p.fetch
      put = putW wr
  run w p (\_, i, wd => case fetch i wd of
             MkIORes v wd1 => case genAt es i wd1 of
               MkIORes g wd2 => case toPrim (f v) wd2 of
                 MkIORes x wd3 => case liveAtGen es i g wd3 of
                   MkIORes True wd4 => put i x wd4
                   MkIORes False wd4 => MkIORes () wd4) ()

||| Visit every matching entity for its effect.
export covering
cfor : {0 q : Type} -> Query cs q => World cs -> (q -> IO ()) -> IO ()
cfor w f = do
  p <- plan {q} w
  let fetch = p.fetch
  run w p (\_, i, wd => case fetch i wd of
             MkIORes v wd' => toPrim (f v) wd') ()

||| Fold over every matching entity, without building a list of them.
export covering
cfold : {0 q : Type} -> Query cs q => World cs -> (acc -> q -> acc) -> acc -> IO acc
cfold w f z = do
  p <- plan {q} w
  let fetch = p.fetch
  run w p (\acc, i, wd => case fetch i wd of
             MkIORes v wd' => MkIORes (f acc v) wd') z

||| Spawn an entity holding a bundle -- any write type, so a tuple of
||| components. `Nothing` when the world is full.
export
spawn : {0 b : Type} -> Write cs b => World cs -> b -> IO (Maybe Entity)
spawn w bundle = do
  me <- spawnEmpty w
  case me of
    Nothing => pure Nothing
    Just e => do
      wr <- write {r = b} w
      fromPrim (putW wr (entityIndex e) bundle)
      pure (Just e)

||| Add or replace a bundle on an existing entity. No-op on a stale one.
export
insertAll : {0 b : Type} -> Write cs b => World cs -> Entity -> b -> IO ()
insertAll w e bundle = do
  ok <- alive w e
  when ok $ do
    wr <- write {r = b} w
    fromPrim (putW wr (entityIndex e) bundle)
