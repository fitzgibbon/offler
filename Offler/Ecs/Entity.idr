||| Entities: generational indices into a world of fixed capacity, bevy's
||| `Entity` and the same bargain as `MeshHandle` and `Voice`. Despawning
||| bumps the slot's generation and recycles its index, so a stale handle
||| -- one whose index now names someone else -- matches nothing and every
||| operation on it is a no-op.
|||
||| An entity is two `Int`s, never one packed `Int`: `Int` is 32 bits on the
||| JavaScript backend, too narrow to share between an index and a
||| generation.
module Offler.Ecs.Entity

import Data.IORef
import Offler.Ecs.Column

%default total

||| The constructor is private: entities are minted by `alloc` alone, so an
||| entity's index is always below the capacity of the world that made it.
export
data Entity : Type where
  MkEntity : (idx : Int) -> (gen : Int) -> Entity

export %inline
entityIndex : Entity -> Int
entityIndex (MkEntity i _) = i

export %inline
entityGen : Entity -> Int
entityGen (MkEntity _ g) = g

export
Eq Entity where
  MkEntity i g == MkEntity j h = i == j && g == h

export
Show Entity where
  show (MkEntity i g) = show i ++ "v" ++ show g

||| For `Offler.Ecs` only: rebuild the live entity at an index the stores
||| hand back.
export %inline
entityAt : (idx : Int) -> (gen : Int) -> Entity
entityAt = MkEntity

||| The allocator. `gens` holds each slot's current generation, `alive` is
||| 1 for a live slot, and `free` is a stack of recycled indices.
export
record Entities where
  constructor MkEntities
  cap : Int
  gens : I32Array
  alive : I32Array
  free : I32Array
  freeTop : IORef Int
  fresh : IORef Int
  live : IORef Int

export
newEntities : (cap : Int) -> IO Entities
newEntities cap =
  MkEntities cap <$> newI32 cap <*> newI32 cap <*> newI32 cap
                 <*> newIORef 0 <*> newIORef 0 <*> newIORef 0

export %inline
capacity : Entities -> Int
capacity es = es.cap

||| Live entities.
export
liveCount : Entities -> IO Int
liveCount es = readIORef es.live

||| One past the highest index ever allocated: the range an all-entities
||| scan walks.
export
highWater : Entities -> IO Int
highWater es = readIORef es.fresh

||| A recycled index if there is one, else a fresh one; `Nothing` when the
||| world is full.
export
alloc : Entities -> IO (Maybe Entity)
alloc es = do
  top <- readIORef es.freeTop
  i <- if top > 0
         then do writeIORef es.freeTop (top - 1)
                 readI es.free (top - 1)
         else do n <- readIORef es.fresh
                 if n >= es.cap
                   then pure (-1)
                   else writeIORef es.fresh (n + 1) >> pure n
  if i < 0
    then pure Nothing
    else do
      writeI es.alive i 1
      modifyIORef es.live (+ 1)
      g <- readI es.gens i
      pure (Just (MkEntity i g))

||| Whether a slot index is live, whatever its generation.
export %inline
aliveAt : Entities -> Int -> PrimIO Bool
aliveAt es i w = case peekI es.alive i w of
  MkIORes a w' => MkIORes (nonZero a) w'

||| Whether this exact entity is still live: its slot is occupied and at
||| the generation the handle was minted with.
export
isAlive : Entities -> Entity -> IO Bool
isAlive es (MkEntity i g) =
  if i < 0 || i >= es.cap
    then pure False
    else do
      a <- readI es.alive i
      cur <- readI es.gens i
      pure (nonZero a && eqI cur g)

||| A slot's current generation.
export %inline
genAt : Entities -> Int -> PrimIO Int
genAt es = peekI es.gens

||| Whether a slot is live at the given generation.
export %inline
liveAtGen : Entities -> Int -> Int -> PrimIO Bool
liveAtGen es i g w = case peekI es.alive i w of
  MkIORes a w1 => case peekI es.gens i w1 of
    MkIORes cur w2 => MkIORes (nonZero a && eqI cur g) w2

||| The live entity at an index, at its current generation.
export
entityOf : Entities -> Int -> IO Entity
entityOf es i = MkEntity i <$> readI es.gens i

||| Retire a live entity: bump its generation and push its index for
||| reuse. Callers check `isAlive` first.
export
release : Entities -> Entity -> IO ()
release es (MkEntity i g) = do
  writeI es.alive i 0
  writeI es.gens i (g + 1)
  top <- readIORef es.freeTop
  writeI es.free top i
  writeIORef es.freeTop (top + 1)
  modifyIORef es.live (\n => n - 1)
