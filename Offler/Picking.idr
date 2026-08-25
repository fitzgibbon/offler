||| Picking: which thing is under the pointer — offler's `bevy_picking`,
||| in bevy's three layers, none of which touches a render backend.
|||
||| *Pointers.* Mice and fingers unify into `PointerId`, read straight from
||| the `Platform` event stream. A locked pointer has no position, so
||| picking naturally idles under pointer lock.
|||
||| *Backend.* `pickRay` inverts the *actual* camera transform — the same
||| `Projection` the renderer builds its matrix from, against the live
||| surface size — so a pixel maps to a world ray exactly, at every aspect
||| ratio, with no screen-space approximations to drift at the edges.
||| Targets are bounds (`BoundSphere`, `BoundBox`) under world matrices:
||| bevy's raycast backend reads retained `Mesh` assets, but offler's
||| meshes are GPU-side and opaque, so the honest CPU-side shape is a
||| declared bound — the coarse phase of bevy's raycast, which its mesh
||| pass also runs first. `sceneTargets` reads world matrices from the
||| scene graph, `target` takes them straight from whatever a frame drew.
|||
||| *Focus.* `pickStep` is a pure state machine over the frame's events,
||| emitting bevy's interaction vocabulary — `PickOver`/`PickOut` on hover
||| change, `PickDown`/`PickUp`, and `PickClick` when press and release
||| land on the same target. `Picker` wraps it in an `IORef` for scenes
||| that want one call a frame.
module Offler.Picking

import Data.IORef
import Data.List
import Data.Vect
import Offler.Camera
import Offler.Gfx.Platform
import Offler.Math
import Offler.Scene
import Offler.Transform

%default covering

--------------------------------------------------------------------------------
-- Pointers

||| The mouse, or one finger: bevy's `PointerId`.
public export
data PointerId = MousePointer | TouchPointer Int

public export
Eq PointerId where
  MousePointer == MousePointer = True
  TouchPointer a == TouchPointer b = a == b
  _ == _ = False

--------------------------------------------------------------------------------
-- Rays

||| A world-space ray: origin plus unit direction.
public export
record Ray where
  constructor MkRay
  origin : V3
  dir : V3

||| A hit, in world terms: distance along the ray and the point reached.
public export
record Hit where
  constructor MkHit
  distance : Double
  point : V3

along : Ray -> Double -> Hit
along r t = MkHit t (add3 r.origin (scale3 t r.dir))

||| Unproject a surface pixel (drawing-buffer coordinates, the space every
||| pointer event reports in) through the camera into a world ray — the
||| exact inverse of what the renderer does at `beginFrame`, built
||| analytically from the same `Projection` description rather than by
||| inverting a matrix. Perspective rays fan out from the eye; orthographic
||| rays march parallel from the view plane. The camera's scale is ignored,
||| as the view matrix ignores it.
public export
pickRay : Camera -> (surface : (Double, Double)) -> (px, py : Double) -> Ray
pickRay cam (sw, sh) px py =
  let w = max 1.0 sw
      h = max 1.0 sh
      aspect = w / h
      ndcX = 2.0 * px / w - 1.0
      ndcY = 1.0 - 2.0 * py / h
      q = cam.transform.rotation
      eye = cam.transform.translation
   in case cam.projection of
        Perspective fovY _ _ =>
          let t = tan (fovY * 0.5)
              view = MkV3 (ndcX * t * aspect) (ndcY * t) (-1.0)
           in MkRay eye (normalize3 (qRotate q view))
        Orthographic halfH _ _ =>
          let right = qRotate q (MkV3 1.0 0.0 0.0)
              up = qRotate q (MkV3 0.0 1.0 0.0)
              fwd = qRotate q (MkV3 0.0 0.0 (-1.0))
              o = add3 eye (add3 (scale3 (ndcX * halfH * aspect) right)
                                 (scale3 (ndcY * halfH) up))
           in MkRay o fwd

--------------------------------------------------------------------------------
-- Bounds and intersections

||| What a target occupies, in its own local space; its world matrix places
||| it. A unit-sphere mesh drawn at scale s is exactly
||| `BoundSphere zero3 1.0` under the same matrix.
public export
data Bound : Type where
  BoundSphere : (center : V3) -> (radius : Double) -> Bound
  ||| An axis-aligned box, `min` and `max` corners. Under a rotating world
  ||| matrix it is tested as the world-space box of its transformed
  ||| corners — conservative, which is what a coarse pick phase wants.
  BoundBox : (lo : V3) -> (hi : V3) -> Bound

||| Nearest non-negative intersection with a world-space sphere. Inside
||| counts: the exit point is returned, as bevy's raycast does.
public export
rayVsSphere : Ray -> (center : V3) -> (radius : Double) -> Maybe Double
rayVsSphere r c radius =
  let oc = sub3 r.origin c
      b = dot3 oc r.dir
      cc = dot3 oc oc - radius * radius
      disc = b * b - cc
   in if disc < 0.0 then Nothing
      else let s = sqrt disc
               t0 = -b - s
               t1 = -b + s
            in if t0 >= 0.0 then Just t0
               else if t1 >= 0.0 then Just t1
               else Nothing

||| Nearest non-negative intersection with a world-space axis-aligned box,
||| by slabs. A ray starting inside reports distance 0.
public export
rayVsBox : Ray -> (lo : V3) -> (hi : V3) -> Maybe Double
rayVsBox r lo hi =
  let (n0, f0) = slab r.origin.vx r.dir.vx lo.vx hi.vx
      (n1, f1) = slab r.origin.vy r.dir.vy lo.vy hi.vy
      (n2, f2) = slab r.origin.vz r.dir.vz lo.vz hi.vz
      tn = max n0 (max n1 n2)
      tf = min f0 (min f1 f2)
   in if tf >= max tn 0.0 then Just (max tn 0.0) else Nothing
  where
    ||| Entry and exit of one axis' slab; a parallel ray outside its slab
    ||| yields an empty interval.
    slab : (o, d, mn, mx : Double) -> (Double, Double)
    slab o d mn mx =
      if abs d < 1.0e-12
        then if o < mn || o > mx then (1.0, 0.0) else (-1.0e300, 1.0e300)
        else let t1 = (mn - o) / d
                 t2 = (mx - o) / d
              in (min t1 t2, max t1 t2)

||| Intersection with the plane through `p0` with normal `n` (front or
||| back face — picking does not cull).
public export
rayVsPlane : Ray -> (p0 : V3) -> (n : V3) -> Maybe Double
rayVsPlane r p0 n =
  let denom = dot3 n r.dir
   in if abs denom < 1.0e-12 then Nothing
      else let t = dot3 n (sub3 p0 r.origin) / denom
            in if t >= 0.0 then Just t else Nothing

--------------------------------------------------------------------------------
-- Targets

||| One pickable thing: a caller-chosen key, the world matrix it was drawn
||| under this frame, and its local bound.
public export
record PickTarget k where
  constructor MkPickTarget
  key : k
  world : Mat4
  bound : Bound

||| The direct spelling, for frames that already hold their matrices.
public export
target : k -> Mat4 -> Bound -> PickTarget k
target = MkPickTarget

mulPoint : Mat4 -> V3 -> V3
mulPoint m (MkV3 x y z) =
  MkV3 (m.m0 * x + m.m4 * y + m.m8 * z + m.m12)
       (m.m1 * x + m.m5 * y + m.m9 * z + m.m13)
       (m.m2 * x + m.m6 * y + m.m10 * z + m.m14)

||| The largest of the matrix's basis-column lengths: what a radius scales
||| by, exact for uniform scale and conservative otherwise.
scaleOf : Mat4 -> Double
scaleOf m =
  max (length3 (MkV3 m.m0 m.m1 m.m2))
      (max (length3 (MkV3 m.m4 m.m5 m.m6))
           (length3 (MkV3 m.m8 m.m9 m.m10)))

||| The eight corners of a box. `Vect 8` rather than `List`, so the caller
||| below has no empty case to answer for: a box always has corners.
corners : V3 -> V3 -> Vect 8 V3
corners lo hi =
  [ MkV3 lo.vx lo.vy lo.vz, MkV3 lo.vx lo.vy hi.vz
  , MkV3 lo.vx hi.vy lo.vz, MkV3 lo.vx hi.vy hi.vz
  , MkV3 hi.vx lo.vy lo.vz, MkV3 hi.vx lo.vy hi.vz
  , MkV3 hi.vx hi.vy lo.vz, MkV3 hi.vx hi.vy hi.vz ]

vmin : V3 -> V3 -> V3
vmin (MkV3 a b c) (MkV3 d e f) = MkV3 (min a d) (min b e) (min c f)

vmax : V3 -> V3 -> V3
vmax (MkV3 a b c) (MkV3 d e f) = MkV3 (max a d) (max b e) (max c f)

||| Test one target: its bound, placed by its world matrix.
public export
rayVsTarget : Ray -> PickTarget k -> Maybe Double
rayVsTarget r t = case t.bound of
  BoundSphere c radius =>
    rayVsSphere r (mulPoint t.world c) (radius * scaleOf t.world)
  BoundBox lo hi =>
    let (p :: ps) = map (mulPoint t.world) (corners lo hi)
     in rayVsBox r (foldl vmin p ps) (foldl vmax p ps)

||| The backend proper: every target against one ray, nearest hit wins.
public export
castRay : Ray -> List (PickTarget k) -> Maybe (k, Hit)
castRay r = foldl nearer Nothing
  where
    nearer : Maybe (k, Hit) -> PickTarget k -> Maybe (k, Hit)
    nearer acc t = case rayVsTarget r t of
      Nothing => acc
      Just d => case acc of
        Just (_, h) => if h.distance <= d then acc
                       else Just (t.key, along r d)
        Nothing => Just (t.key, along r d)

||| Pixel to nearest target, in one call: `pickRay` then `castRay`.
public export
pickAt : Camera -> (surface : (Double, Double)) -> List (PickTarget k)
      -> (px, py : Double) -> Maybe (k, Hit)
pickAt cam sfc ts px py = castRay (pickRay cam sfc px py) ts

||| Pickable bounds attached to scene nodes: the world matrices come from
||| the graph — `worldOf`, the per-node `GlobalTransform` — so a node
||| picked is a node exactly where the propagation pass drew it. Nodes
||| that have despawned simply drop out.
export
sceneTargets : Scene -> List (k, NodeId, Bound) -> IO (List (PickTarget k))
sceneTargets s entries = do
  ms <- traverse look entries
  pure (mapMaybe id ms)
  where
    look : (k, NodeId, Bound) -> IO (Maybe (PickTarget k))
    look (key, node, b) = do
      mw <- worldOf s node
      pure (map (\w => MkPickTarget key w b) mw)

--------------------------------------------------------------------------------
-- Focus: hover and click state, bevy's pointer events

||| The interaction vocabulary, per pointer. `PickClick` is bevy's rule:
||| press and release on the *same* target.
public export
data PickEvent k
  = PickOver PointerId k Hit
  | PickOut PointerId k
  | PickMove PointerId k Hit
  | PickDown PointerId Button k Hit
  | PickUp PointerId Button k Hit
  | PickClick PointerId Button k Hit

||| What the state machine remembers between frames: where each pointer
||| hovers, and what it pressed on.
export
record PickState k where
  constructor MkPickState
  hover : List (PointerId, k)
  held : List (PointerId, Button, k)

export
initPickState : PickState k
initPickState = MkPickState [] []

||| Current hover, for scenes that want the fact rather than the edges.
export
hoverOf : Eq k => PickState k -> PointerId -> Maybe k
hoverOf st p = lookup p st.hover

||| One frame of the focus layer: fold the platform's events through a
||| query (partially-applied `pickAt`, typically), producing the new state
||| and the interaction events in order. Pure, so a frame's picking is
||| replayable; `Picker` is the `IORef` convenience over it.
export
pickStep : Eq k => ((Double, Double) -> Maybe (k, Hit))
        -> PickState k -> List Event -> (PickState k, List (PickEvent k))
pickStep {k} query st0 evs =
  let (st, out) = foldl one (st0, []) evs
   in (st, reverse out)
  where
    ||| Re-aim a pointer: Out the old hover, Over the new, in that order.
    hoverAt : PointerId -> Maybe (k, Hit)
          -> (PickState k, List (PickEvent k)) -> (PickState k, List (PickEvent k))
    hoverAt p now (st, out) =
      let old = lookup p st.hover
          rest = filter (\(q, _) => q /= p) st.hover
       in case (old, now) of
            (Nothing, Nothing) => (st, out)
            (Just o, Nothing) => ({ hover := rest } st, PickOut p o :: out)
            (Nothing, Just (key, h)) =>
              ({ hover := (p, key) :: rest } st, PickOver p key h :: out)
            (Just o, Just (key, h)) =>
              if o == key
                then (st, PickMove p key h :: out)
                else ({ hover := (p, key) :: rest } st,
                      PickOver p key h :: PickOut p o :: out)

    press : PointerId -> Button -> Maybe (k, Hit)
          -> (PickState k, List (PickEvent k)) -> (PickState k, List (PickEvent k))
    press p b (Just (key, h)) (st, out) =
      ({ held := (p, b, key) :: st.held } st, PickDown p b key h :: out)
    press _ _ Nothing acc = acc

    release : PointerId -> Button -> Maybe (k, Hit)
            -> (PickState k, List (PickEvent k)) -> (PickState k, List (PickEvent k))
    release p b now (st, out) =
      let was = map (\(_, _, key) => key)
                    (find (\(q, c, _) => q == p && c == b) st.held)
          st' = the (PickState k)
                    ({ held := filter (\(q, c, _) => not (q == p && c == b)) st.held } st)
       in case now of
            Nothing => (st', out)
            Just (key, h) =>
              let up = PickUp p b key h :: out
               in if was == Just key
                    then (st', PickClick p b key h :: up)
                    else (st', up)

    ||| A finger that lifted is gone: close its hover too.
    vanish : PointerId
           -> (PickState k, List (PickEvent k)) -> (PickState k, List (PickEvent k))
    vanish p (st, out) =
      case lookup p st.hover of
        Nothing => (st, out)
        Just o => ({ hover := filter (\(q, _) => q /= p) st.hover } st,
                   PickOut p o :: out)

    one : (PickState k, List (PickEvent k)) -> Event
       -> (PickState k, List (PickEvent k))
    one acc (PointerMove x y) = hoverAt MousePointer (query (x, y)) acc
    one acc (PointerDown b x y) =
      let hit = query (x, y)
       in press MousePointer b hit (hoverAt MousePointer hit acc)
    one acc (PointerUp b x y) =
      let hit = query (x, y)
       in release MousePointer b hit (hoverAt MousePointer hit acc)
    one acc (TouchStart i x y) =
      let p = TouchPointer i
          hit = query (x, y)
       in press p LeftButton hit (hoverAt p hit acc)
    one acc (TouchMove i x y) = hoverAt (TouchPointer i) (query (x, y)) acc
    one acc (TouchEnd i x y) =
      let p = TouchPointer i
       in vanish p (release p LeftButton (query (x, y)) acc)
    one acc _ = acc

--------------------------------------------------------------------------------
-- The IORef convenience

export
record Picker k where
  constructor MkPicker
  state : IORef (PickState k)

export
newPicker : IO (Picker k)
newPicker = MkPicker <$> newIORef initPickState

||| The one-call-a-frame form: this frame's camera, surface size, targets
||| and events in; interaction events out, hover and press state kept.
export
pickEvents : Eq k => Picker k -> Camera -> (surface : (Double, Double))
          -> List (PickTarget k) -> List Event -> IO (List (PickEvent k))
pickEvents pk cam sfc ts evs = do
  st <- readIORef pk.state
  let (st', out) = pickStep (\(x, y) => pickAt cam sfc ts x y) st evs
  writeIORef pk.state st'
  pure out

||| What a pointer currently hovers, from the kept state.
export
hovered : Eq k => Picker k -> PointerId -> IO (Maybe k)
hovered pk p = do
  st <- readIORef pk.state
  pure (hoverOf st p)
