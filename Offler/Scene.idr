||| A retained scene graph: a tree of transforms, some nodes carrying a
||| drawable, with world transforms computed by a propagation pass.
|||
||| This is bevy's `bevy_transform` + `bevy_hierarchy`, minus the ECS. In
||| bevy those crates *host* the hierarchy in the ECS (`ChildOf` and
||| `Children` are components, propagation is a system writing
||| `GlobalTransform`), but nothing about the idea needs an ECS and nothing
||| about it touches the render backend: the renderer merely consumes the
||| propagated world transforms. Here the same structure is a plain
||| retained tree behind an `IORef`, and the render side of the seam is one
||| function: `collect` flattens the tree to `(world matrix, drawable)`
||| pairs, which `Offler.Gfx.Renderer.drawAll` draws. The graph knows
||| nothing about backends; the backends know nothing about the graph.
|||
||| Nodes hold `Transform`s relative to their parent, bevy's `Transform`
||| convention; `collect` computes what bevy calls `GlobalTransform`. There
||| is no dirty tracking: propagation is a full multiply-down each call,
||| which at scene-graph granularity (dozens to hundreds of nodes) is
||| nothing. Crowds do not belong in the graph -- batch them with
||| `drawMany`, as the Swarm example does.
module Offler.Scene

import Data.IORef
import Data.SortedMap
import Offler.Gfx.Renderer
import Offler.Math
import Offler.Transform

%default covering

||| A node in the graph. The constructor is private: nodes are made by
||| `spawn` and named by `NodeId`, which cannot be forged.
record Node where
  constructor MkNode
  transform : Transform
  drawable : Maybe Drawable
  parent : Maybe Int
  children : List Int

export
data NodeId : Type where
  MkNodeId : Int -> NodeId

export
record Scene where
  constructor MkScene
  nodes : IORef (SortedMap Int Node)
  roots : IORef (List Int)
  nextId : IORef Int

export
newScene : IO Scene
newScene = MkScene <$> newIORef empty <*> newIORef [] <*> newIORef 0

||| `SortedMap` has no `adjust`; update-if-present.
adjust : Int -> (Node -> Node) -> SortedMap Int Node -> SortedMap Int Node
adjust k f m = case lookup k m of
                 Just v => insert k (f v) m
                 Nothing => m

||| Add a node under a parent (or at the root), with a local transform and
||| optionally something to draw. `Drawable`'s constructor already carries
||| the mesh/material compatibility proof, so an ill-topologied node cannot
||| be built, let alone spawned.
export
spawn : Scene -> Maybe NodeId -> Transform -> Maybe Drawable -> IO NodeId
spawn s parent tf dr = do
  i <- readIORef s.nextId
  writeIORef s.nextId (i + 1)
  let pid = map (\(MkNodeId p) => p) parent
  modifyIORef s.nodes (insert i (MkNode tf dr pid []))
  case pid of
    Nothing => modifyIORef s.roots (i ::)
    Just p => modifyIORef s.nodes (adjust p ({ children $= (i ::) }))
  pure (MkNodeId i)

||| Replace a node's local transform: the per-frame animation entry point.
export
setTransform : Scene -> NodeId -> Transform -> IO ()
setTransform s (MkNodeId i) tf =
  modifyIORef s.nodes (adjust i ({ transform := tf }))

export
getTransform : Scene -> NodeId -> IO (Maybe Transform)
getTransform s (MkNodeId i) = do
  ns <- readIORef s.nodes
  pure (map (.transform) (lookup i ns))

||| Replace (or clear) what a node draws.
export
setDrawable : Scene -> NodeId -> Maybe Drawable -> IO ()
setDrawable s (MkNodeId i) dr =
  modifyIORef s.nodes (adjust i ({ drawable := dr }))

||| Remove a node and its whole subtree.
export
despawn : Scene -> NodeId -> IO ()
despawn s (MkNodeId i) = do
  ns <- readIORef s.nodes
  case lookup i ns of
    Nothing => pure ()
    Just n => do
      traverse_ (\c => despawn s (MkNodeId c)) n.children
      modifyIORef s.nodes (delete i)
      case n.parent of
        Nothing => modifyIORef s.roots (filter (/= i))
        Just p => modifyIORef s.nodes (adjust p ({ children $= filter (/= i) }))

||| The propagation pass: flatten the tree into draw order, each node's
||| world matrix the product of its ancestors' -- `GlobalTransform`,
||| computed rather than cached.
export
collect : Scene -> IO (List (Mat4, Drawable))
collect s = do
  ns <- readIORef s.nodes
  rs <- readIORef s.roots
  pure (concatMap (go ns identity) (reverse rs))
  where
    go : SortedMap Int Node -> Mat4 -> Int -> List (Mat4, Drawable)
    go ns parentM i =
      case lookup i ns of
        Nothing => []
        Just n =>
          let world = parentM `mmul` matOf n.transform
              here = case n.drawable of
                       Just d => [(world, d)]
                       Nothing => []
           in here ++ concatMap (go ns world) (reverse n.children)

||| Propagate and draw, in one call: what a frame does with a scene.
export
renderScene : Renderer r f => r -> (1 frame : f) -> Scene -> L1 IO f
renderScene r fr s = do
  ds <- liftIO (collect s)
  drawAll r fr ds
