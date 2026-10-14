||| Drawing a world: per-entity draws for props, and instanced batches for
||| crowds whose component column *is* the GPU instance buffer.
|||
||| `Instance b` is one instance's model matrix and colour, laid out
||| exactly as `Offler.Gfx.Layout.instanceFields` describes, in a packed
||| store. Because a sparse set keeps its dense run contiguous, the store's
||| float column is already a filled staging buffer: `uploadBatch` hands it
||| to `writeInstances` as it stands, with no extraction pass. The phantom
||| `b` names the batch, so a world holds one instance column per crowd and
||| the type checker keeps them apart.
module Offler.Ecs.Render

import Offler.Color
import Offler.Ecs.Query
import Offler.Ecs.Store
import Offler.Ecs.Transform
import Offler.Ecs.World
import Offler.Gfx.Layout
import Offler.Gfx.Renderer
import Offler.Math

%default covering

||| Something to draw at the entity's `GlobalTransform`.
public export
record Renderable where
  constructor MkRenderable
  drawable : Drawable

export
Component Renderable where kind = Boxed

||| One member of the instanced batch `b`.
public export
record Instance (0 b : Type) where
  constructor MkInstance
  model : Mat4
  tint : Color

||| An instance's floats: the model matrix, then the colour at
||| `instColorFloat`.
public export
instanceWidth : Nat
instanceWidth = 20

0 instanceWidthOk : the Int (cast Offler.Ecs.Render.instanceWidth) = Offler.Gfx.Layout.instanceFloats
instanceWidthOk = Refl

0 instanceColorOk : the Int (cast (the Nat 16)) = Offler.Gfx.Layout.instColorFloat
instanceColorOk = Refl

export
Columnar (Instance b) where
  width = instanceWidth
  storeCol a o (MkInstance m c) = do
    pokeMat a (field 0 o) m
    poke4 a (field 16 o) c.red c.green c.blue c.alpha
  loadCol a o = do
    m0 <- peek a (field 0 o); m1 <- peek a (field 1 o)
    m2 <- peek a (field 2 o); m3 <- peek a (field 3 o)
    m4 <- peek a (field 4 o); m5 <- peek a (field 5 o)
    m6 <- peek a (field 6 o); m7 <- peek a (field 7 o)
    m8 <- peek a (field 8 o); m9 <- peek a (field 9 o)
    m10 <- peek a (field 10 o); m11 <- peek a (field 11 o)
    m12 <- peek a (field 12 o); m13 <- peek a (field 13 o)
    m14 <- peek a (field 14 o); m15 <- peek a (field 15 o)
    r <- peek a (field 16 o); g <- peek a (field 17 o)
    bl <- peek a (field 18 o); al <- peek a (field 19 o)
    pure (MkInstance (MkMat4 m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 m13 m14 m15)
                     (MkColor r g bl al))

||| Always packed: the store's column is what `uploadBatch` uploads.
export
Component (Instance b) where kind = Packed

||| Upload batch `b`'s instance column to a GPU instance buffer, as it
||| stands. Then `drawInstanced` with that buffer draws the whole batch.
export
uploadBatch : Renderer r f => (0 b : Type) -> {auto el : Has (Instance b) cs}
           -> r -> InstanceHandle -> World cs -> IO ()
uploadBatch _ r ih w = do
  st <- storeOf w el
  _ <- withColumn st $ \col, n => writeInstances r ih (columnSlice col n)
  pure ()

||| Every renderable entity with its world matrix, as `drawAll` takes them.
export
drawables : {auto gl : Has GlobalTransform cs} -> {auto rl : Has Renderable cs}
         -> World cs -> IO (List (Mat4, Drawable))
drawables w =
  cfold {q = (GlobalTransform, Renderable)} w
    (\acc, (MkGlobal m, MkRenderable d) => (m, d) :: acc) []

||| Draw every renderable entity: `Offler.Scene.renderScene` for a world.
export
renderWorld : Renderer r f => {auto gl : Has GlobalTransform cs}
           -> {auto rl : Has Renderable cs}
           -> r -> (1 frame : f) -> World cs -> L1 IO f
renderWorld r fr w = do
  ds <- liftIO (drawables w)
  drawAll r fr ds
