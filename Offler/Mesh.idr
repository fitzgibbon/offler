||| Mesh primitives, mirroring bevy's `primitives` module: cuboid, sphere,
||| plane, torus, cylinder, cone, circle, rectangle. Each is a plain triangle
||| list built on the CPU; `uploadMesh` flattens it into the `Verts` a
||| renderer's `createMesh` takes.
|||
||| Every vertex carries a position, a normal and texture coordinates.
||| Winding is counter-clockwise seen from outside, which is what the
||| pipelines cull against.
module Offler.Mesh

import Data.Nat
import Data.Vect

import Offler.Gfx.Array
import Offler.Color
import Offler.Gfx.Layout
import Offler.Math
import Offler.Vect

%default total

-- `Data.Vect`'s own map/append/concat cons, and V8 overflows on them between
-- 5 000 and 8 000 elements -- which the meshes below pass (a 64x64 torus is
-- 8 192 triangles). `Offler.Vect` has the accumulator-passing versions; a
-- `%transform` applies only in the module that declares it and only to a
-- local right-hand side, so the aliases below exist to be that side. See
-- `Offler.Vect` for why the Prelude needs none of this for lists.
mapL : (a -> b) -> Vect n a -> Vect n b
mapL = mapV

appendL : Vect m a -> Vect n a -> Vect (m + n) a
appendL = appendV

concatL : {0 a : Type} -> {0 n : Nat} -> Vect m (Vect n a) -> Vect (m * n) a
concatL = concatV

toListL : Vect n a -> List a
toListL = toListV

%transform "meshVectMap"    Prelude.Interfaces.map {f = Vect n} g v = mapL g v
%transform "meshVectAppend" Data.Vect.(++) xs ys = appendL xs ys
%transform "meshVectConcat" Data.Vect.concat v = concatL v
%transform "meshVectToList" Prelude.Interfaces.toList {t = Vect n} v = toListL v

public export
record Vertex where
  constructor MkVertex
  position, normal : V3
  uv : V2

public export
Tri : Type
Tri = (Vertex, Vertex, Vertex)

||| Triangles, counted. The count is erased, so a `MeshData n` is the same
||| cons cells a `List Tri` was; what it adds is that `uploadMesh` allocates
||| for exactly the triangles it is given, and every primitive below
||| publishes how many it produces.
public export
MeshData : Nat -> Type
MeshData n = Vect n Tri

||| A quad from its centre and two half-edge vectors, normal `u x v`
||| (normalised), corners `c ± u ± v` wound counter-clockwise from the
||| normal's side, with `u` and `v` spanning the texture square.
quadFace : (center, u, v : V3) -> MeshData 2
quadFace c u v =
  let n = normalize3 (cross3 u v)
      a = MkVertex (sub3 (sub3 c u) v) n (MkV2 0.0 1.0)
      b = MkVertex (sub3 (add3 c u) v) n (MkV2 1.0 1.0)
      d = MkVertex (add3 (add3 c u) v) n (MkV2 1.0 0.0)
      e = MkVertex (add3 (sub3 c u) v) n (MkV2 0.0 0.0)
   in [(a, b, d), (a, d, e)]

--------------------------------------------------------------------------------
-- 3D primitives

||| An axis-aligned box of the given full extents, centred at the origin.
||| Each face maps the whole texture.
public export
cuboid : (width, height, depth : Double) -> MeshData 12
cuboid w h d =
  let hx = w * 0.5; hy = h * 0.5; hz = d * 0.5
   in quadFace (MkV3 hx 0.0 0.0) (MkV3 0.0 0.0 (-hz)) (MkV3 0.0 hy 0.0)
   ++ quadFace (MkV3 (-hx) 0.0 0.0) (MkV3 0.0 0.0 hz) (MkV3 0.0 hy 0.0)
   ++ quadFace (MkV3 0.0 hy 0.0) (MkV3 0.0 0.0 hz) (MkV3 hx 0.0 0.0)
   ++ quadFace (MkV3 0.0 (-hy) 0.0) (MkV3 hx 0.0 0.0) (MkV3 0.0 0.0 hz)
   ++ quadFace (MkV3 0.0 0.0 hz) (MkV3 hx 0.0 0.0) (MkV3 0.0 hy 0.0)
   ++ quadFace (MkV3 0.0 0.0 (-hz)) (MkV3 0.0 hy 0.0) (MkV3 hx 0.0 0.0)

||| A unit cube.
public export
cube : MeshData 12
cube = cuboid 1.0 1.0 1.0

||| A flat rectangle in the XZ plane facing +y: the ground.
public export
plane : (width, depth : Double) -> MeshData 2
plane w d = quadFace zero3 (MkV3 0.0 0.0 (d * 0.5)) (MkV3 (w * 0.5) 0.0 0.0)

||| A rectangle in the XY plane facing +z: the 2D quad.
public export
rectangle : (width, height : Double) -> MeshData 2
rectangle w h = quadFace zero3 (MkV3 (w * 0.5) 0.0 0.0) (MkV3 0.0 (h * 0.5) 0.0)

||| Texture coordinates for a point on the unit sphere: the spherical
||| projection, `u` around the equator and `v` pole to pole. An icosphere
||| triangle whose vertices straddle the seam smears -- the classic ico
||| seam, which bevy's icosphere shares.
sphereUV : V3 -> V2
sphereUV p =
  MkV2 (0.5 + atan2 p.vz p.vx / tau)
       (0.5 - asin (max (-1.0) (min 1.0 p.vy)) / pi)

||| The twenty faces of a regular icosahedron on the unit sphere, from three
||| golden rectangles. Every vertex doubles as its own normal.
|||
||| The vertices are a `Vect 12` and the face table indexes it with `Fin 12`,
||| so a typo in the twenty triples below is a compile error. As `List Int`
||| it was not: the lookup answered an out-of-range index with `zero3`, and a
||| slipped digit became a degenerate triangle at the origin that renders as
||| a barely-visible sliver with nothing anywhere reporting it.
icosahedron : Vect 20 (V3, V3, V3)
icosahedron =
  let p = (1.0 + sqrt 5.0) / 2.0
      v = map onSphere $ the (Vect 12 V3)
            [ MkV3 (-1.0) p 0.0, MkV3 1.0 p 0.0, MkV3 (-1.0) (-p) 0.0, MkV3 1.0 (-p) 0.0
            , MkV3 0.0 (-1.0) p, MkV3 0.0 1.0 p, MkV3 0.0 (-1.0) (-p), MkV3 0.0 1.0 (-p)
            , MkV3 p 0.0 (-1.0), MkV3 p 0.0 1.0, MkV3 (-p) 0.0 (-1.0), MkV3 (-p) 0.0 1.0
            ]
      idx = the (Vect 20 (Fin 12, Fin 12, Fin 12))
            [ (0,11,5),(0,5,1),(0,1,7),(0,7,10),(0,10,11)
            , (1,5,9),(5,11,4),(11,10,2),(10,7,6),(7,1,8)
            , (3,9,4),(3,4,2),(3,2,6),(3,6,8),(3,8,9)
            , (4,9,5),(2,4,11),(6,2,10),(8,6,7),(9,8,1)
            ]
   in map (\(a, b, c) => (index a v, index b v, index c v)) idx

||| Split each triangle into four, pushing the new midpoints out to the
||| sphere.
subdivide : Vect m (V3, V3, V3) -> Vect (m * 4) (V3, V3, V3)
subdivide v = concatV (mapV split v)
  where
    split : (V3, V3, V3) -> Vect 4 (V3, V3, V3)
    split (a, b, c) =
      let ab = midpoint a b
          bc = midpoint b c
          ca = midpoint c a
       in [(a, ab, ca), (ab, b, bc), (ca, bc, c), (ab, bc, ca)]

||| Triangles in an icosphere at a subdivision level: twenty, quadrupling.
public export
faceCount : Nat -> Nat
faceCount Z = 20
faceCount (S k) = 4 * faceCount k

unitSphere : (n : Nat) -> Vect (faceCount n) (V3, V3, V3)
unitSphere Z = icosahedron
unitSphere (S k) =
  rewrite multCommutative 4 (faceCount k) in subdivide (unitSphere k)

||| An icosphere: `subdivisions` 0 is 20 triangles, each level quadruples.
||| 3 (1280 triangles) reads as smooth at ordinary sizes. UVs are the
||| spherical projection, with the usual ico seam.
public export
sphere : (radius : Double) -> (subdivisions : Nat) -> MeshData (faceCount subdivisions)
sphere r n = mapV (\(a, b, c) => (vert a, vert b, vert c)) (unitSphere n)
  where
    vert : V3 -> Vertex
    vert p = MkVertex (scale3 r p) p (sphereUV p)

||| A torus around +y: `ringRadius` from the centre to the middle of the
||| tube, `tubeRadius` of the tube itself. The texture wraps once around
||| each way. Two triangles per grid cell.
|||
||| The segment counts are `Nat` with an `LTE 3` bound rather than `Int`
||| silently clamped by `max 3`: asking for two segments is now a compile
||| error instead of quietly getting three.
public export
torus : (ringRadius, tubeRadius : Double)
     -> (ringSegments, tubeSegments : Nat)
     -> {auto 0 ru : LTE 3 ringSegments} -> {auto 0 tv : LTE 3 tubeSegments}
     -> MeshData (ringSegments * (tubeSegments * 2))
torus rr tr nu nv =
  let du = tau / cast nu
      dv = tau / cast nv
      vert = \u, v => let n = MkV3 (cos u * cos v) (sin v) (sin u * cos v)
                          c = MkV3 (rr * cos u) 0.0 (rr * sin u)
                       in MkVertex (add3 c (scale3 tr n)) n
                                   (MkV2 (u / tau) (v / tau))
   in tabulateFlat nu $ \i => tabulateFlat nv $ \j =>
        let u = cast i * du
            u' = cast (i + 1) * du
            v = cast j * dv
            v' = cast (j + 1) * dv
            a = vert u v
            b = vert u v'
            c = vert u' v'
            d = vert u' v
         in [(a, b, c), (a, c, d)]

||| A cylinder around +y, centred at the origin, with caps. The side wraps
||| the texture once around; the caps map it radially. Two side triangles
||| and two cap triangles per segment.
public export
cylinder : (radius, height : Double) -> (segments : Nat)
        -> {auto 0 ok : LTE 3 segments} -> MeshData (segments * 4)
cylinder r h n =
  let du = tau / cast n
      hy = h * 0.5
      up = MkV3 0.0 1.0 0.0
      down = MkV3 0.0 (-1.0) 0.0
      capUV = \uu => MkV2 (0.5 + 0.5 * cos uu) (0.5 + 0.5 * sin uu)
   in tabulateFlat n $ \i =>
        let u = cast i * du
            u' = cast (i + 1) * du
            na = MkV3 (cos u) 0.0 (sin u)
            nb = MkV3 (cos u') 0.0 (sin u')
            a = MkVertex (MkV3 (r * cos u) (-hy) (r * sin u)) na (MkV2 (u / tau) 1.0)
            b = MkVertex (MkV3 (r * cos u) hy (r * sin u)) na (MkV2 (u / tau) 0.0)
            c = MkVertex (MkV3 (r * cos u') hy (r * sin u')) nb (MkV2 (u' / tau) 0.0)
            d = MkVertex (MkV3 (r * cos u') (-hy) (r * sin u')) nb (MkV2 (u' / tau) 1.0)
            pt = \uu, yy => MkV3 (r * cos uu) yy (r * sin uu)
            centreUV = MkV2 0.5 0.5
         in [ (a, b, c), (a, c, d)
            , ( MkVertex (MkV3 0.0 hy 0.0) up centreUV
              , MkVertex (pt u' hy) up (capUV u')
              , MkVertex (pt u hy) up (capUV u) )
            , ( MkVertex (MkV3 0.0 (-hy) 0.0) down centreUV
              , MkVertex (pt u (-hy)) down (capUV u)
              , MkVertex (pt u' (-hy)) down (capUV u') ) ]

||| A cone around +y: base at -height/2, apex at +height/2, with a base cap.
||| One side triangle and one cap triangle per segment.
public export
cone : (radius, height : Double) -> (segments : Nat)
    -> {auto 0 ok : LTE 3 segments} -> MeshData (segments * 2)
cone r h n =
  let du = tau / cast n
      hy = h * 0.5
      slant = \u => normalize3 (MkV3 (cos u * h) r (sin u * h))
      apex = MkV3 0.0 hy 0.0
      base = \u => MkV3 (r * cos u) (-hy) (r * sin u)
      down = MkV3 0.0 (-1.0) 0.0
      capUV = \uu => MkV2 (0.5 + 0.5 * cos uu) (0.5 + 0.5 * sin uu)
   in tabulateFlat n $ \i =>
        let u = cast i * du
            u' = cast (i + 1) * du
         in [ ( MkVertex apex (slant ((u + u') * 0.5)) (MkV2 ((u + u') * 0.5 / tau) 0.0)
              , MkVertex (base u') (slant u') (MkV2 (u' / tau) 1.0)
              , MkVertex (base u) (slant u) (MkV2 (u / tau) 1.0) )
            , ( MkVertex (MkV3 0.0 (-hy) 0.0) down (MkV2 0.5 0.5)
              , MkVertex (base u) down (capUV u)
              , MkVertex (base u') down (capUV u') ) ]

||| A filled circle in the XY plane facing +z, texture mapped radially.
||| One triangle per segment.
public export
circle : (radius : Double) -> (segments : Nat)
      -> {auto 0 ok : LTE 3 segments} -> MeshData segments
circle r n =
  let du = tau / cast n
      fwd = MkV3 0.0 0.0 1.0
      pt = \u => MkV3 (r * cos u) (r * sin u) 0.0
      uvAt = \u => MkV2 (0.5 + 0.5 * cos u) (0.5 - 0.5 * sin u)
   in tabulate n $ \i =>
        let u = cast i * du
            u' = cast (i + 1) * du
         in ( MkVertex zero3 fwd (MkV2 0.5 0.5)
            , MkVertex (pt u) fwd (uvAt u)
            , MkVertex (pt u') fwd (uvAt u') )

--------------------------------------------------------------------------------
-- Upload

||| Flatten a triangle list into the buffer a renderer's `createMesh` takes:
||| position, normal and uv, eight floats a vertex, twenty-four a triangle
||| written as six `poke4`s.
|||
||| Every write is *proved* in bounds, not tested: the buffer is allocated
||| as exactly `n * 3` vertices, triangle `i`'s position comes from
||| `strided` under `countLT`'s loop invariant, and the offsets inside a
||| triangle are erased `sub` arithmetic. The `window` test this loop used
||| to run could not fail -- the buffer was sized from the same length --
||| and its `Nothing` branch silently truncated the mesh if it ever had.
||| Now there is no branch.
export
uploadMesh : {n : Nat} -> MeshData n -> IO (VertBuf Triangles)
uploadMesh {n} mesh = do
  (arr, h) <- newVertsAt {t = Triangles} (n * 3)
  fromPrim (go arr 0 Refl mesh)
  pure (MkVertBuf _ arr h)
  where
    vert : F32Array cap -> At cap 8 -> Vertex -> IO ()
    vert arr o (MkVertex p nm t) = do
      poke4 arr (sub 0 o) p.vx p.vy p.vz nm.vx
      poke4 arr (sub 4 o) nm.vy nm.vz t.vx t.vy

    tri : F32Array cap -> At cap 24 -> Tri -> IO ()
    tri arr o (a, b, c) = do
      vert arr (sub 0 o) a
      vert arr (sub 8 o) b
      vert arr (sub 16 o) c

    at : (i : Nat) -> (0 ok : LT i n) -> At ((n * 3) * 8) 24
    at i ok = rewrite sym (multAssociative n 3 8) in strided i ok

    -- The world bound on the left, as in `Offler.Gfx.Uniform`'s batch
    -- loops: an IO do-block here compiles to a non-tail call and V8
    -- overflows past ~65 000 elements; this form gets `__tailRec`.
    go : F32Array ((n * 3) * 8) -> (done : Nat) -> (0 inv : done + k = n)
      -> Vect k Tri -> PrimIO ()
    go arr _ _ [] w = MkIORes () w
    go arr done inv ((::) {len} t rest) w =
      case toPrim (tri arr (at done (countLT done inv)) t) w of
        MkIORes _ w' =>
          go arr (S done) (trans (plusSuccRightSucc done len) inv) rest w'

||| Upload shared vertices and the index list naming them: how a
||| parametric grid wants to exist -- each interior vertex stored once and
||| named by six triangle corners. The indices are `Fin` over the vertex
||| count, so every one names a vertex that is actually there; the vertex
||| writes are proved in bounds the same way `uploadMesh`'s are.
export
uploadIndexed : {n : Nat} -> Vect n Vertex -> Vect k (Fin n)
             -> IO (VertBuf Triangles, Indices)
uploadIndexed {n} verts idxs = do
  (arr, h) <- newVertsAt {t = Triangles} n
  fromPrim (goV arr 0 Refl verts)
  ix <- newIndices (cast (lengthV idxs))
  fromPrim (goI ix 0 idxs)
  pure (MkVertBuf _ arr h, ix)
  where
    vert : F32Array (n * 8) -> At (n * 8) 8 -> Vertex -> IO ()
    vert arr o (MkVertex p nm t) = do
      poke4 arr (sub 0 o) p.vx p.vy p.vz nm.vx
      poke4 arr (sub 4 o) nm.vy nm.vz t.vx t.vy

    goV : F32Array (n * 8) -> (done : Nat) -> (0 inv : done + j = n)
       -> Vect j Vertex -> PrimIO ()
    goV arr _ _ [] w = MkIORes () w
    goV arr done inv ((::) {len} v rest) w =
      case toPrim (vert arr (strided done (countLT done inv)) v) w of
        MkIORes _ w' =>
          goV arr (S done) (trans (plusSuccRightSucc done len) inv) rest w'

    goI : Indices -> Int -> Vect j (Fin n) -> PrimIO ()
    goI _ _ [] w = MkIORes () w
    goI ix i (v :: rest) w =
      case toPrim (pokeIndex ix i (cast (finToNat v))) w of
        MkIORes _ w' => goI ix (i + 1) rest w'

||| A parametric grid of `(nu+1) x (nv+1)` shared vertices, triangulated
||| with two triangles per cell: the indexed form of every
||| surface-of-revolution.
|||
||| The indices are `Fin` over the vertex count, so an index cannot name a
||| vertex that is not there. As raw `Int` they could: the four corner
||| expressions below are ordinary arithmetic, and a slipped `+ 1` produced
||| an out-of-range index that `pokeIndex` answered by dropping the write --
||| geometry visibly wrong, with nothing reporting it at any layer. The
||| bound now comes from `Offler.Vect.pairIndex`, which proves
||| `i * row + j < rows * row` once.
public export
indexedGrid : (nu, nv : Nat) -> (Nat -> Nat -> Vertex)
           -> ( Vect (S nu * S nv) Vertex
              , Vect (nu * (nv * 6)) (Fin (S nu * S nv)) )
indexedGrid nu nv f =
  ( tabulateFlat (S nu) (\i => tabulate (S nv) (\j => f i j))
  , tabulateFinFlat nu $ \i => tabulateFinFlat nv $ \j =>
      let a = pairIndex (weaken i) (weaken j)
          b = pairIndex (weaken i) (FS j)
          c = pairIndex (FS i) (FS j)
          d = pairIndex (FS i) (weaken j)
       in [a, b, c, a, c, d] )

||| The torus as an indexed grid: shared vertices, proper seam UVs (the
||| duplicated seam row carries u=1 where the first carries u=0).
public export
torusIndexed : (ringRadius, tubeRadius : Double)
            -> (ringSegments, tubeSegments : Nat)
            -> {auto 0 ru : LTE 3 ringSegments} -> {auto 0 tv : LTE 3 tubeSegments}
            -> ( Vect (S ringSegments * S tubeSegments) Vertex
               , Vect (ringSegments * (tubeSegments * 6))
                      (Fin (S ringSegments * S tubeSegments)) )
torusIndexed rr tr nu nv =
  indexedGrid nu nv $ \i, j =>
    let u = cast i * (tau / cast nu)
        v = cast j * (tau / cast nv)
        n = MkV3 (cos u * cos v) (sin v) (sin u * cos v)
        c = MkV3 (rr * cos u) 0.0 (rr * sin u)
     in MkVertex (add3 c (scale3 tr n)) n
                 (MkV2 (cast i / cast nu) (cast j / cast nv))

--------------------------------------------------------------------------------
-- Lines

||| Colour a segment list uniformly.
public export
colored : Color -> Vect n (V3, V3) -> Vect n (V3, V3, Color)
colored c = mapV (\(a, b) => (a, b, c))

||| Coloured line segments -- eight floats a vertex (vec4 position, vec4
||| colour), so one `poke16` fills a whole segment -- into the buffer
||| `setGizmos` or a `Lines` `createMesh` takes. Writes proved in bounds,
||| as in `uploadMesh`.
export
uploadLines : {n : Nat} -> Vect n (V3, V3, Color) -> IO (VertBuf Lines)
uploadLines {n} segs = do
  (arr, h) <- newVertsAt {t = Lines} (n * 2)
  fromPrim (go arr 0 Refl segs)
  pure (MkVertBuf _ arr h)
  where
    at : (i : Nat) -> (0 ok : LT i n) -> At ((n * 2) * 8) 16
    at i ok = rewrite sym (multAssociative n 2 8) in strided i ok

    go : F32Array ((n * 2) * 8) -> (done : Nat) -> (0 inv : done + k = n)
      -> Vect k (V3, V3, Color) -> PrimIO ()
    go arr _ _ [] w = MkIORes () w
    go arr done inv ((::) {len} (a, b, c) rest) w =
      case toPrim (poke16 arr (at done (countLT done inv))
                     a.vx a.vy a.vz 0.0  c.red c.green c.blue c.alpha
                     b.vx b.vy b.vz 0.0  c.red c.green c.blue c.alpha) w of
        MkIORes _ w' =>
          go arr (S done) (trans (plusSuccRightSucc done len) inv) rest w'

||| Consecutive points joined into segments: `n + 1` points give `n`
||| segments, which the type now says.
public export
polyline : Vect (S n) V3 -> Vect n (V3, V3)
polyline (x :: xs) = go x xs
  where
    go : V3 -> Vect k V3 -> Vect k (V3, V3)
    go _ [] = []
    go prev (y :: ys) = (prev, y) :: go y ys

||| A closed loop of the given points: the polyline plus the closing
||| segment, so `n + 1` points give `n + 1` segments.
public export
loop : Vect (S n) V3 -> Vect (S n) (V3, V3)
loop {n} pts =
  replace {p = \k => Vect k (V3, V3)} (plusCommutative n 1)
          (appendV (polyline pts) [closing])
  where
    closing : (V3, V3)
    closing = (Data.Vect.last pts, Data.Vect.head pts)

||| A square grid in the XZ plane: `2n+1` lines each way, `spacing` apart,
||| so `4n + 2` segments in all.
public export
gridXZ : (n : Nat) -> (spacing : Double) -> Vect ((n + (n + 1)) * 2) (V3, V3)
gridXZ n spacing =
  let ext = cast n * spacing
   in tabulateFlat (n + (n + 1)) $ \i =>
        let d = (cast i - cast n) * spacing
         in [ (MkV3 (-ext) 0.0 d, MkV3 ext 0.0 d)
            , (MkV3 d 0.0 (-ext), MkV3 d 0.0 ext) ]
