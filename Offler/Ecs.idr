||| An entity-component-system: APECS's core in bevy's vocabulary.
|||
||| * `Offler.Ecs.World` -- the world, components and their storage kinds
||| * `Offler.Ecs.Query` -- systems: `cmap`, `cmapM`, `cfor`, `cfold`, `spawn`
||| * `Offler.Ecs.Transform` -- `Transform`, `GlobalTransform`, `ChildOf`, `propagate`
||| * `Offler.Ecs.Render` -- `Renderable`, zero-copy `Instance` batches
||| * `Offler.Ecs.Schedule` -- `Tick`, `Clock`, schedules with per-system change ticks
||| * `Offler.Ecs.Event` -- `Events`, `sendEvent`, `readEvents`
||| * `Offler.Ecs.Picking` -- `Pickable`, `pickTargets`
module Offler.Ecs

import public Offler.Ecs.Entity
import public Offler.Ecs.Event
import public Offler.Ecs.Picking
import public Offler.Ecs.Query
import public Offler.Ecs.Render
import public Offler.Ecs.Schedule
import public Offler.Ecs.Store
import public Offler.Ecs.Transform
import public Offler.Ecs.World
