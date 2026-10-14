||| Picking entities: `Offler.Picking`'s ray casts against bounds carried
||| as components and placed by each entity's `GlobalTransform` -- the same
||| world matrix the renderer draws with, so what is picked is what is on
||| screen. `sceneTargets` for a world.
module Offler.Ecs.Picking

import Offler.Ecs.Entity
import Offler.Ecs.Query
import Offler.Ecs.Transform
import Offler.Ecs.World
import public Offler.Picking

%default covering

||| A pick bound in the entity's local space.
public export
record Pickable where
  constructor MkPickable
  bound : Bound

export
Component Pickable where kind = Boxed

||| Every pickable entity as a pick target keyed by the entity.
export
pickTargets : {auto gl : Has GlobalTransform cs} -> {auto pl : Has Pickable cs}
           -> World cs -> IO (List (PickTarget Entity))
pickTargets w =
  cfold {q = (Entity, GlobalTransform, Pickable)} w
    (\acc, (e, MkGlobal m, MkPickable b) => target e m b :: acc) []
