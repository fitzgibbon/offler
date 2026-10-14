||| The world: an entity allocator and one store per component type, with
||| the set of component types in the world's own type.
|||
||| `World cs` is indexed by its component list, so asking for a component
||| the world was not built with is an unsolvable `Elem` -- a compile
||| error, where bevy panics on an unregistered component at run time.
||| Each component chooses its storage through its `Component` instance,
||| as APECS components do, so the world needs no run-time type ids and no
||| archetype moves: a component insert touches one store.
|||
||| What the world holds is data and stores only. Systems, schedules and
||| any closure that captures the world live outside it -- a world that
||| held a closure over itself would be a reference cycle, which a
||| backend without a tracing collector cannot reclaim.
module Offler.Ecs.World

import Data.IORef
import public Data.List.Elem
import public Data.List.Quantifiers
import public Offler.Ecs.Entity
import public Offler.Ecs.Store
import Offler.Ecs.Column

%default total

||| A type that can be stored in a world, and how.
public export
interface Component (0 c : Type) where
  kind : Kind c

||| A world-wide value: bevy's `Res`. Every resource is a component stored
||| `Global`, through the one instance below, so `resource` can demand
||| `Resource c` and know the store it finds holds exactly one value. A
||| type cannot be both: a hand-written `Component` instance for a
||| resource type makes the two instances ambiguous.
public export
interface Resource (0 c : Type) where
  initial : c

export
Resource c => Component c where
  kind = Global (initial {c})

||| Membership of a component type in a world's list: the proof is the
||| store's position, found by proof search at the call site.
public export
Has : Type -> List Type -> Type
Has c cs = Elem c cs

||| Every type in the list is a component: the stores a world can build.
public export
interface Components (0 cs : List Type) where
  newStores : (cap : Int) -> IO (All Store cs)

export
Components [] where
  newStores _ = pure []

export
(Component c, Components cs) => Components (c :: cs) where
  newStores cap = [| newStore (kind {c}) cap :: newStores {cs} cap |]

export
record World (cs : List Type) where
  constructor MkWorld
  entities : Entities
  stores : All Store cs
  ||| Free snapshot buffers. A query borrows one for its duration and
  ||| returns it, so nested queries each get their own.
  scratch : IORef (List I32Array)
  ||| The change tick writes are stamped with. Starts at 1, so everything
  ||| written before any system ran is newer than a system that never has.
  tickNow : IORef Int
  ||| The tick the running system last ran at: what `Changed`, `Added`
  ||| and `readEvents` compare against. 0 outside a schedule.
  tickSeen : IORef Int
  ||| The tick this frame began at, and the one before it: events older
  ||| than the previous frame's start are pruned.
  frameStart : IORef Int
  frameFloor : IORef Int

||| A world with room for `cap` live entities at once. Every column is
||| allocated here, at its full size: nothing grows, so nothing a query
||| holds is ever reallocated beneath it.
export
newWorld : Components cs => (cap : Int) -> IO (World cs)
newWorld cap = do
  let n = max 1 cap
  MkWorld <$> newEntities n <*> newStores {cs} n <*> newIORef []
          <*> newIORef 1 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0

||| The tick writes are stamped with now.
export
currentTick : World cs -> IO Int
currentTick w = readIORef w.tickNow

||| The running system's last-run tick.
export
seenTick : World cs -> IO Int
seenTick w = readIORef w.tickSeen

||| For `Offler.Ecs.Schedule`: enter a system that last ran at `seen`,
||| advancing the tick so its writes are newer than everything before.
||| Returns the tick it runs at.
export
enterSystem : World cs -> (seen : Int) -> IO Int
enterSystem w seen = do
  t <- readIORef w.tickNow
  writeIORef w.tickNow (t + 1)
  writeIORef w.tickSeen seen
  pure (t + 1)

||| For `Offler.Ecs.Schedule`: leave a system. The tick advances past it,
||| so writes made outside any system -- input sent between frames, setup
||| -- are newer than every system's last run, the last one's included.
export
leaveSystem : World cs -> IO ()
leaveSystem w = do
  modifyIORef w.tickNow (+ 1)
  writeIORef w.tickSeen 0

||| For `Offler.Ecs.Schedule`: a new frame begins. From here on, events
||| sent before the previous frame began are pruned, so every system sees
||| an event at least once whatever its place in the order.
export
startFrame : World cs -> IO ()
startFrame w = do
  floor <- readIORef w.frameStart
  readIORef w.tickNow >>= writeIORef w.frameStart
  writeIORef w.frameFloor floor

||| For `Offler.Ecs.Event`: the ticks this frame and the previous one
||| began at.
export
frameTicks : World cs -> IO (Int, Int)
frameTicks w = [| MkPair (readIORef w.frameStart) (readIORef w.frameFloor) |]

||| For `Offler.Ecs` modules: the allocator.
export %inline
entitiesOf : World cs -> Entities
entitiesOf w = w.entities

||| For `Offler.Ecs` modules: a component's store, found by its position.
||| An `IO` action on purpose: its result is bound once, by `>>=`, and the
||| per-entity closures built after it capture the store. A pure lookup
||| used once gets inlined into those closures -- and re-walked, `Elem`
||| proof and all, for every entity.
export
storeOf : World cs -> Has c cs -> IO (Store c)
storeOf w el = pure (go w.stores el)
  where
    go : All Store xs -> Elem c xs -> Store c
    go (s :: _) Here = s
    go (_ :: ss) (There e) = go ss e

||| For `Offler.Ecs` modules: run with a snapshot buffer of the world's
||| capacity, borrowed from the free list or made on first need.
export
withScratch : World cs -> (I32Array -> IO a) -> IO a
withScratch w f = do
  free <- readIORef w.scratch
  buf <- case free of
           b :: rest => writeIORef w.scratch rest >> pure b
           [] => newI32 (capacity w.entities)
  r <- f buf
  modifyIORef w.scratch (buf ::)
  pure r

||| Live entities.
export
entityCount : World cs -> IO Int
entityCount w = liveCount w.entities

||| How many live entities the world can hold at once.
export
worldCapacity : World cs -> Int
worldCapacity w = capacity w.entities

||| Entities holding a component.
export
countOf : (0 c : Type) -> {auto el : Has c cs} -> World cs -> IO Int
countOf _ w = storeOf w el >>= size

export
alive : World cs -> Entity -> IO Bool
alive w = isAlive w.entities

||| A new entity with no components; `Nothing` when the world is full.
export
spawnEmpty : World cs -> IO (Maybe Entity)
spawnEmpty w = alloc w.entities

||| Remove an entity and every component it holds. A stale entity is a
||| no-op. Safe inside a query: queries iterate a snapshot.
export
despawn : World cs -> Entity -> IO ()
despawn w e = do
  ok <- isAlive w.entities e
  when ok $ do
    removeAll w.stores (entityIndex e)
    release w.entities e
  where
    removeAll : All Store xs -> Int -> IO ()
    removeAll [] _ = pure ()
    removeAll (s :: ss) i = remove s i >> removeAll ss i

||| Add or replace one component. No-op on a stale entity.
export
insert : {auto el : Has c cs} -> World cs -> Entity -> c -> IO ()
insert w e v = do
  ok <- isAlive w.entities e
  when ok $ do
    st <- storeOf w el
    wr <- currentTick w >>= writer st
    fromPrim (wr (entityIndex e) v)

||| Remove one component, if the entity holds it.
export
removeComponent : (0 c : Type) -> {auto el : Has c cs} -> World cs -> Entity -> IO ()
removeComponent _ w e = do
  ok <- isAlive w.entities e
  when ok $ storeOf w el >>= \st => remove st (entityIndex e)

||| One component of one entity, if it is live and holds it.
export
getComponent : {auto el : Has c cs} -> World cs -> Entity -> IO (Maybe c)
getComponent w e = do
  ok <- isAlive w.entities e
  if not ok then pure Nothing else do
    st <- storeOf w el
    let i = entityIndex e
    present <- member st >>= \m => fromPrim (m i)
    if present then reader st >>= \rd => Just <$> fromPrim (rd i) else pure Nothing

export
holds : (0 c : Type) -> {auto el : Has c cs} -> World cs -> Entity -> IO Bool
holds _ w e = do
  ok <- isAlive w.entities e
  if ok then storeOf w el >>= member >>= \m => fromPrim (m (entityIndex e)) else pure False

||| A resource's value.
export
resource : Resource c => {auto el : Has c cs} -> World cs -> IO c
resource w = do
  st <- storeOf w el
  rd <- reader st
  fromPrim (rd 0)

||| Replace a resource's value, stamping it changed.
export
setResource : Resource c => {auto el : Has c cs} -> World cs -> c -> IO ()
setResource w v = do
  st <- storeOf w el
  wr <- currentTick w >>= writer st
  fromPrim (wr 0 v)

export
modifyResource : Resource c => {auto el : Has c cs} -> World cs -> (c -> c) -> IO ()
modifyResource w f = resource w >>= setResource w . f
