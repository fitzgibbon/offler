||| Gizmos: debug and annotation drawing, offler's `bevy_gizmos`, in both
||| of bevy's modes.
|||
||| *Immediate*: build a `GizmoData` — a pure list of coloured segments,
||| where bevy accumulates into a `Gizmos` system param — from the
||| vocabulary below (`gLine`, `gArrow`, `gAxes`, `gCircle`, `gSphere`,
||| `gCuboid`, `gGrid`, ...), concatenate freely, and hand the frame's
||| worth to `drawGizmoData` before `beginFrame`. The renderer draws the
||| whole overlay in one blended call via `drawGizmos`.
|||
||| *Retained* (bevy 0.14's `GizmoAsset` + `Gizmo` component): freeze a
||| `GizmoData` with `gizmoAsset` and spawn the resulting `Drawable` into
||| the scene like anything else — it then moves by node transform, costs
||| no rebuild, and keeps its *per-vertex colours*, because it draws
||| through `GizmoMaterial`, an ordinary engine-shipped `Material` whose
||| `vs_line` forwards them (nothing privileged: application code could
||| define it). Segments are in *object* space for retained gizmos (the
||| node transform places them) and world space for the immediate overlay.
module Offler.Gizmos

import Offler.Color
import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Gfx.Material
import Offler.Gfx.Renderer
import Offler.Gfx.Uniform
import Data.Nat
import Data.Vect

import Offler.Math
import Offler.Mesh
import Offler.Vect

%default covering

-- As in `Offler.Mesh`: base's `Vect` map/append/concat cons, so V8 overflows
-- on them past a few thousand elements, and a `%transform` only applies in
-- the module that declares it, to a local right-hand side. See `Offler.Vect`.
mapL : (a -> b) -> Vect n a -> Vect n b
mapL = mapV

appendL : Vect m a -> Vect n a -> Vect (m + n) a
appendL = appendV

concatL : {0 a : Type} -> {0 n : Nat} -> Vect m (Vect n a) -> Vect (m * n) a
concatL = concatV

%transform "gizmoVectMap"    Prelude.Interfaces.map {f = Vect n} g v = mapL g v
%transform "gizmoVectAppend" Data.Vect.(++) xs ys = appendL xs ys
%transform "gizmoVectConcat" Data.Vect.concat v = concatL v

||| Coloured segments, counted. `++` still composes them -- `Vect`'s append
||| adds the indices -- so the concatenation vocabulary is unchanged; what
||| the index adds is that `uploadLines` allocates for exactly the segments
||| it is handed.
public export
GizmoData : Nat -> Type
GizmoData n = Vect n (V3, V3, Color)

||| Gizmo data whose length is only known at run time: an overlay
||| accumulated from a variable number of sources -- one ring per ripple
||| still fading, one marker per selected thing. The builders below all have
||| static counts, so they say so; this is what you fold them into when the
||| *number of builders* is itself dynamic. `uploadLines` still allocates for
||| exactly the segments present, because the count travels in the pair.
public export
Gizmos : Type
Gizmos = (n : Nat ** GizmoData n)

||| A statically-counted batch as a dynamic one.
public export
some : {n : Nat} -> GizmoData n -> Gizmos
some {n} g = (n ** g)

public export
noGizmos : Gizmos
noGizmos = (0 ** [])

||| Concatenate two dynamic batches; the counts add.
public export
(<+>) : Gizmos -> Gizmos -> Gizmos
(<+>) (m ** a) (n ** b) = (m + n ** appendV a b)

||| Fold a list of batches into one -- the `concatMap` of the dynamic form.
public export
gconcat : List Gizmos -> Gizmos
gconcat = foldl (<+>) noGizmos

--------------------------------------------------------------------------------
-- The vocabulary

public export
gLine : Color -> V3 -> V3 -> GizmoData 1
gLine c a b = [(a, b, c)]

||| A ray: origin along a (not necessarily unit) direction.
public export
gRay : Color -> (origin : V3) -> (dir : V3) -> GizmoData 1
gRay c o d = [(o, add3 o d, c)]

||| Consecutive points joined, one colour.
public export
gPolyline : Color -> Vect (S n) V3 -> GizmoData n
gPolyline c ps = colored c (polyline ps)

||| A line with a two-segment head at the tip.
public export
||| A line with a two-segment head at the tip. A degenerate arrow (`from`
||| and `to` within 1e-4) yields three zero-length segments rather than
||| none, so the count is three either way and `gAxes` below can say nine:
||| a zero-length segment draws nothing, which is what an arrow to nowhere
||| should do.
gArrow : Color -> (from : V3) -> (to : V3) -> GizmoData 3
gArrow c from to =
  let d = sub3 to from
      len = length3 d
   in if len < 0.0001 then [(from, from, c), (from, from, c), (from, from, c)] else
      let dir = scale3 (1.0 / len) d
          -- Any vector not parallel to dir gives a perpendicular.
          ref = if abs dir.vy < 0.9 then MkV3 0.0 1.0 0.0 else MkV3 1.0 0.0 0.0
          side = normalize3 (cross3 dir ref)
          back = scale3 (len * 0.85) dir
          w = len * 0.06
          p1 = add3 from (add3 back (scale3 w side))
          p2 = add3 from (sub3 back (scale3 w side))
       in [(from, to, c), (to, p1, c), (to, p2, c)]

||| The classic RGB basis at a transform-free origin: X red, Y green,
||| Z blue — bevy's `Gizmos::axes`.
public export
gAxes : (origin : V3) -> (size : Double) -> GizmoData 9
gAxes o s =
     gArrow (srgb 0.9 0.2 0.2) o (add3 o (MkV3 s 0.0 0.0))
  ++ gArrow (srgb 0.2 0.9 0.2) o (add3 o (MkV3 0.0 s 0.0))
  ++ gArrow (srgb 0.25 0.4 0.95) o (add3 o (MkV3 0.0 0.0 s))

||| A circle around `axis` (need not be unit) at `center`.
public export
gCircle : Color -> (center : V3) -> (axis : V3) -> (radius : Double)
       -> (segments : Nat) -> {auto 0 ok : LTE 3 segments} -> GizmoData segments
gCircle c center axis radius n =
  let ax = normalize3 axis
      ref = if abs ax.vy < 0.9 then MkV3 0.0 1.0 0.0 else MkV3 1.0 0.0 0.0
      u = normalize3 (cross3 ax ref)
      v = cross3 ax u
      pt = \k => let a = cast k * (tau / cast n)
                  in add3 center (add3 (scale3 (radius * cos a) u)
                                       (scale3 (radius * sin a) v))
   in tabulate n (\k => (pt k, pt (k + 1), c))

||| Three orthogonal circles: the wire sphere.
public export
gSphere : Color -> (center : V3) -> (radius : Double) -> (segments : Nat)
       -> {auto 0 ok : LTE 3 segments} -> GizmoData (segments + (segments + segments))
gSphere c o r n =
     gCircle c o (MkV3 1.0 0.0 0.0) r n
  ++ gCircle c o (MkV3 0.0 1.0 0.0) r n
  ++ gCircle c o (MkV3 0.0 0.0 1.0) r n

||| The twelve edges of an axis-aligned box, centred at `center`.
public export
gCuboid : Color -> (center : V3) -> (size : V3) -> GizmoData 12
gCuboid c o s =
  [ e (p (-1)(-1)(-1)) (p 1 (-1)(-1)), e (p 1 (-1)(-1)) (p 1 (-1) 1)
  , e (p 1 (-1) 1) (p (-1)(-1) 1), e (p (-1)(-1) 1) (p (-1)(-1)(-1))
  , e (p (-1) 1 (-1)) (p 1 1 (-1)), e (p 1 1 (-1)) (p 1 1 1)
  , e (p 1 1 1) (p (-1) 1 1), e (p (-1) 1 1) (p (-1) 1 (-1))
  , e (p (-1)(-1)(-1)) (p (-1) 1 (-1)), e (p 1 (-1)(-1)) (p 1 1 (-1))
  , e (p 1 (-1) 1) (p 1 1 1), e (p (-1)(-1) 1) (p (-1) 1 1) ]
  where
    p : Double -> Double -> Double -> V3
    p sx sy sz = add3 o (MkV3 (sx * s.vx * 0.5) (sy * s.vy * 0.5) (sz * s.vz * 0.5))

    e : V3 -> V3 -> (V3, V3, Color)
    e a b = (a, b, c)

||| A square grid in the XZ plane: `2n+1` lines each way.
public export
gGrid : Color -> (n : Nat) -> (spacing : Double)
     -> GizmoData ((n + (n + 1)) * 2)
gGrid c n spacing = colored c (gridXZ n spacing)

||| A small three-axis cross: a point marker.
public export
gCross : Color -> (center : V3) -> (size : Double) -> GizmoData 3
gCross c o s =
  [ (add3 o (MkV3 (-s) 0.0 0.0), add3 o (MkV3 s 0.0 0.0), c)
  , (add3 o (MkV3 0.0 (-s) 0.0), add3 o (MkV3 0.0 s 0.0), c)
  , (add3 o (MkV3 0.0 0.0 (-s)), add3 o (MkV3 0.0 0.0 s), c) ]

--------------------------------------------------------------------------------
-- The two consumers

||| Immediate mode: replace the frame's overlay with this data. Call before
||| `beginFrame` (the upload must not happen mid-pass), then `drawGizmos`
||| inside the frame, after the meshes the blend should sit over.
export
drawGizmoData : Renderer r f => r -> Gizmos -> IO ()
drawGizmoData r (_ ** gd) = do
  buf <- uploadLines gd
  setGizmos r buf.handle

--------------------------------------------------------------------------------
-- The retained gizmo material

||| What retained gizmos draw with: per-vertex colour times a tint, the
||| tint doubling as bevy's per-`Gizmo` colour config (line width and
||| joints are not expressible on hairline WebGL2/WebGPU pipelines). An
||| ordinary `Material` implementation — nothing here is privileged.
public export
record GizmoMaterial where
  constructor MkGizmoMaterial
  tint : Color

public export
Material GizmoMaterial where
  matFields = [MkField "tint" Vec4]
  matTexCount = 0
  matTextureSlots = []
  matLineEntry = True
  alphaMode _ = Blend
  matTextures _ = []
  writeMat w g = putColor w 0 g.tint

  matWgsl = """
    struct VsOut {
      @builtin(position) pos : vec4<f32>,
      @location(0) color : vec4<f32>,
    };

    @vertex
    fn vs_line(v : LineIn) -> VsOut {
      var out : VsOut;
      out.pos = g.proj * g.view * o.model * vec4<f32>(v.pos.xyz, 1.0);
      out.color = v.color * m.tint;
      return out;
    }

    // Triangle meshes get the flat tint: gizmos are a line vocabulary.
    @vertex
    fn vs(v : VertexIn) -> VsOut {
      var out : VsOut;
      out.pos = g.proj * g.view * o.model * vec4<f32>(v.pos, 1.0);
      out.color = m.tint;
      return out;
    }

    @fragment
    fn fs(in : VsOut) -> @location(0) vec4<f32> {
      return vec4<f32>(pow(in.color.rgb, vec3<f32>(0.4545)), in.color.a);
    }
    """

  matGlslVert = """
    #version 300 es
    out vec4 vColor;
    void main() {
      vColor = tint;
      gl_Position = proj * view * model * vec4(pos, 1.0);
    }
    """

  matGlslLineVert = """
    #version 300 es
    out vec4 vColor;
    void main() {
      vColor = color * tint;
      gl_Position = proj * view * model * vec4(pos.xyz, 1.0);
    }
    """

  matGlslFrag = """
    #version 300 es
    in vec4 vColor;
    out vec4 outColour;
    void main() {
      outColour = vec4(pow(vColor.rgb, vec3(0.4545)), vColor.a);
    }
    """

||| Register the gizmo material's pipelines: once, at startup, like any
||| other material type.
export
registerGizmos : Renderer r f => r -> IO (MaterialId GizmoMaterial)
registerGizmos r = registerMaterial {m = GizmoMaterial} r

||| The bare `Lines` mesh, when the data should draw through your own
||| material's `vs_line` instead of `GizmoMaterial`.
export
retainGizmos : Renderer r f => r -> Gizmos -> IO (MeshHandle Lines)
retainGizmos r (_ ** gd) = do
  buf <- uploadLines gd
  createMesh r buf.handle

||| Retained mode, bevy 0.14's `GizmoAsset` + `Gizmo` spawn flow: freeze
||| the data (object space) into a `Lines` mesh, mint a tinted
||| gizmo-material asset, and hand back the `Drawable` a scene node holds.
||| A `white` tint shows the data's own colours verbatim.
export
gizmoAsset : Renderer r f => r -> MaterialId GizmoMaterial -> Color
          -> Gizmos -> IO Drawable
gizmoAsset r gm tint gd = do
  mesh <- retainGizmos r gd
  h <- addMaterial r gm (MkGizmoMaterial tint)
  pure (MkDrawable mesh h)
