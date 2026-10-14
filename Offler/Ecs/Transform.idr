||| Transforms in the world: bevy's `Transform`, `GlobalTransform` and
||| `ChildOf`, and the propagation system between them.
|||
||| `Transform` and `GlobalTransform` are packed: ten and sixteen floats a
||| column. `ChildOf` holds an `Entity`, so it is boxed; a parent is named
||| by id, never by reference, which is what keeps a hierarchy free of
||| reference cycles.
module Offler.Ecs.Transform

import Offler.Ecs.Entity
import Offler.Ecs.Query
import Offler.Ecs.Store
import Offler.Ecs.World
import public Offler.Math
import public Offler.Transform

%default covering

||| The world matrix propagation computes: bevy's `GlobalTransform`.
public export
record GlobalTransform where
  constructor MkGlobal
  matrix : Mat4

||| This entity's `Transform` is relative to the parent's world matrix.
public export
record ChildOf where
  constructor MkChildOf
  parent : Entity

export
Columnar Transform where
  width = 10
  storeCol a o (MkTransform t r s) = do
    poke4 a (field 0 o) t.vx t.vy t.vz r.qx
    poke4 a (field 4 o) r.qy r.qz r.qw s.vx
    poke a (field 8 o) s.vy
    poke a (field 9 o) s.vz
  loadCol a o = do
    tx <- peek a (field 0 o); ty <- peek a (field 1 o); tz <- peek a (field 2 o)
    rx <- peek a (field 3 o); ry <- peek a (field 4 o); rz <- peek a (field 5 o)
    rw <- peek a (field 6 o)
    sx <- peek a (field 7 o); sy <- peek a (field 8 o); sz <- peek a (field 9 o)
    pure (MkTransform (MkV3 tx ty tz) (MkQuat rx ry rz rw) (MkV3 sx sy sz))

export
Columnar GlobalTransform where
  width = 16
  storeCol a o (MkGlobal m) = pokeMat a o m
  loadCol a o = do
    m0 <- peek a (field 0 o); m1 <- peek a (field 1 o)
    m2 <- peek a (field 2 o); m3 <- peek a (field 3 o)
    m4 <- peek a (field 4 o); m5 <- peek a (field 5 o)
    m6 <- peek a (field 6 o); m7 <- peek a (field 7 o)
    m8 <- peek a (field 8 o); m9 <- peek a (field 9 o)
    m10 <- peek a (field 10 o); m11 <- peek a (field 11 o)
    m12 <- peek a (field 12 o); m13 <- peek a (field 13 o)
    m14 <- peek a (field 14 o); m15 <- peek a (field 15 o)
    pure (MkGlobal (MkMat4 m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15))

export
Component Transform where kind = Packed

export
Component GlobalTransform where kind = Packed

export
Component ChildOf where kind = Boxed

||| The components a world needs for `propagate`.
public export
Hierarchy : List Type -> Type
Hierarchy cs = (Has Transform cs, Has GlobalTransform cs, Has ChildOf cs)

||| An entity's world matrix, computed up its parent chain. `Nothing` when
||| the chain reaches a despawned parent or one without a `Transform`, or
||| exceeds the world's capacity in length -- which only a `ChildOf` cycle
||| can do.
export
worldMatrix : Hierarchy cs => World cs -> Entity -> IO (Maybe Mat4)
worldMatrix @{(_, _, _)} w e = go (cast (worldCapacity w)) e
  where
    go : Nat -> Entity -> IO (Maybe Mat4)
    go Z _ = pure Nothing
    go (S k) x = do
      Just t <- getComponent {c = Transform} w x | Nothing => pure Nothing
      Just (MkChildOf p) <- getComponent {c = ChildOf} w x
        | Nothing => pure (Just (matOf t))
      map (`mmul` matOf t) <$> go k p

rootGlobal : (Transform, x, Without ChildOf) -> GlobalTransform
rootGlobal (t, _, _) = MkGlobal (matOf t)

||| Recompute `GlobalTransform`s: a root's when its `Transform` changed
||| since this system last ran (or it has none yet), every child's by
||| walking its parent chain -- a child depends on ancestors whose changes
||| it cannot see, and at hierarchy scale (props, not crowds) the walk is
||| nothing. Run it after the systems that move things and before
||| anything that reads world positions: rendering, picking, attachment.
|||
||| A child whose chain is broken keeps its last `GlobalTransform`.
export
propagate : Hierarchy cs => World cs -> IO ()
propagate @{(_, _, _)} w = do
  cmap {q = (Transform, Changed Transform, Without ChildOf)} w rootGlobal
  cmap {q = (Transform, Without GlobalTransform, Without ChildOf)} w rootGlobal
  cfor {q = (Entity, With ChildOf)} w $ \(e, _) => do
    Just m <- worldMatrix w e | Nothing => pure ()
    insert w e (MkGlobal m)

||| Re-insert an entity's `Transform` as it stands, marking it changed.
touch : Hierarchy cs => World cs -> Entity -> IO ()
touch @{(_, _, _)} w e = do
  Just t <- getComponent {c = Transform} w e | Nothing => pure ()
  insert w e t

||| Make `child`'s `Transform` relative to `parent`.
export
setParent : Hierarchy cs => World cs -> (child, parent : Entity) -> IO ()
setParent @{h@(_, _, _)} w c p = insert w c (MkChildOf p) >> touch @{h} w c

||| Make an entity a root again. Its `Transform` is marked changed, so the
||| next `propagate` recomputes it as one.
export
detach : Hierarchy cs => World cs -> Entity -> IO ()
detach @{h@(_, _, _)} w e = removeComponent ChildOf w e >> touch @{h} w e
