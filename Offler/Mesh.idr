||| Mesh primitives, mirroring bevy's `primitives` module: cuboid, sphere,
||| plane, torus, cylinder, cone, circle, rectangle. Each is a plain triangle
||| list built on the CPU; `uploadMesh` flattens it into the `Verts` a
||| renderer's `createMesh` takes.
|||
||| Winding is counter-clockwise seen from outside, which is what the
||| pipelines cull against.
module Offler.Mesh

import Offler.Gfx.Array
import Offler.Gfx.Layout
import Offler.Math

%default total

public export
record Vertex where
  constructor MkVertex
  position, normal : V3

public export
Tri : Type
Tri = (Vertex, Vertex, Vertex)

public export
MeshData : Type
MeshData = List Tri

||| A quad from its centre and two half-edge vectors, normal `u x v`
||| (normalised), corners `c ± u ± v` wound counter-clockwise from the
||| normal's side.
quadFace : (center, u, v : V3) -> List Tri
quadFace c u v =
  let n = normalize3 (cross3 u v)
      a = MkVertex (sub3 (sub3 c u) v) n
      b = MkVertex (sub3 (add3 c u) v) n
      d = MkVertex (add3 (add3 c u) v) n
      e = MkVertex (add3 (sub3 c u) v) n
   in [(a, b, d), (a, d, e)]

--------------------------------------------------------------------------------
-- 3D primitives

||| An axis-aligned box of the given full extents, centred at the origin.
public export
cuboid : (width, height, depth : Double) -> MeshData
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
cube : MeshData
cube = cuboid 1.0 1.0 1.0

||| A flat rectangle in the XZ plane facing +y: the ground.
public export
plane : (width, depth : Double) -> MeshData
plane w d = quadFace zero3 (MkV3 0.0 0.0 (d * 0.5)) (MkV3 (w * 0.5) 0.0 0.0)

||| A rectangle in the XY plane facing +z: the 2D quad.
public export
rectangle : (width, height : Double) -> MeshData
rectangle w h = quadFace zero3 (MkV3 (w * 0.5) 0.0 0.0) (MkV3 0.0 (h * 0.5) 0.0)

||| The twenty faces of a regular icosahedron on the unit sphere, from three
||| golden rectangles. Every vertex doubles as its own normal.
icosahedron : List (V3, V3, V3)
icosahedron =
  let p = (1.0 + sqrt 5.0) / 2.0
      v = map onSphere
            [ MkV3 (-1.0) p 0.0, MkV3 1.0 p 0.0, MkV3 (-1.0) (-p) 0.0, MkV3 1.0 (-p) 0.0
            , MkV3 0.0 (-1.0) p, MkV3 0.0 1.0 p, MkV3 0.0 (-1.0) (-p), MkV3 0.0 1.0 (-p)
            , MkV3 p 0.0 (-1.0), MkV3 p 0.0 1.0, MkV3 (-p) 0.0 (-1.0), MkV3 (-p) 0.0 1.0
            ]
      idx = the (List (Int, Int, Int))
            [ (0,11,5),(0,5,1),(0,1,7),(0,7,10),(0,10,11)
            , (1,5,9),(5,11,4),(11,10,2),(10,7,6),(7,1,8)
            , (3,9,4),(3,4,2),(3,2,6),(3,6,8),(3,8,9)
            , (4,9,5),(2,4,11),(6,2,10),(8,6,7),(9,8,1)
            ]
      get : Int -> V3
      get n = nth v n
   in map (\(a, b, c) => (get a, get b, get c)) idx
  where
    nth : List V3 -> Int -> V3
    nth [] _ = zero3
    nth (x :: xs) i = if i <= 0 then x else nth xs (i - 1)

||| Split each triangle into four, pushing the new midpoints out to the
||| sphere.
subdivide : List (V3, V3, V3) -> List (V3, V3, V3)
subdivide = concatMap split
  where
    split : (V3, V3, V3) -> List (V3, V3, V3)
    split (a, b, c) =
      let ab = midpoint a b
          bc = midpoint b c
          ca = midpoint c a
       in [(a, ab, ca), (ab, b, bc), (ca, bc, c), (ab, bc, ca)]

unitSphere : Nat -> List (V3, V3, V3)
unitSphere Z = icosahedron
unitSphere (S k) = subdivide (unitSphere k)

||| An icosphere: `subdivisions` 0 is 20 triangles, each level quadruples.
||| 3 (1280 triangles) reads as smooth at ordinary sizes.
public export
sphere : (radius : Double) -> (subdivisions : Nat) -> MeshData
sphere r n =
  map (\(a, b, c) => (vert a, vert b, vert c)) (unitSphere n)
  where
    vert : V3 -> Vertex
    vert p = MkVertex (scale3 r p) p

||| A torus around +y: `ringRadius` from the centre to the middle of the
||| tube, `tubeRadius` of the tube itself.
public export
torus : (ringRadius, tubeRadius : Double) -> (ringSegments, tubeSegments : Int) -> MeshData
torus rr tr ringSegments tubeSegments =
  let nu = max 3 ringSegments
      nv = max 3 tubeSegments
      du = tau / cast nu
      dv = tau / cast nv
      vert = \u, v => let n = MkV3 (cos u * cos v) (sin v) (sin u * cos v)
                          c = MkV3 (rr * cos u) 0.0 (rr * sin u)
                       in MkVertex (add3 c (scale3 tr n)) n
   in concatMap (\i => concatMap (\j =>
        let u = cast i * du
            u' = cast (i + 1) * du
            v = cast j * dv
            v' = cast (j + 1) * dv
            a = vert u v
            b = vert u v'
            c = vert u' v'
            d = vert u' v
         in [(a, b, c), (a, c, d)])
        (range 0 (nv - 1)))
        (range 0 (nu - 1))

||| A cylinder around +y, centred at the origin, with caps.
public export
cylinder : (radius, height : Double) -> (segments : Int) -> MeshData
cylinder r h segments =
  let n = max 3 segments
      du = tau / cast n
      hy = h * 0.5
      side = \i => let u = cast i * du
                       u' = cast (i + 1) * du
                       na = MkV3 (cos u) 0.0 (sin u)
                       nb = MkV3 (cos u') 0.0 (sin u')
                       a = MkVertex (MkV3 (r * cos u) (-hy) (r * sin u)) na
                       b = MkVertex (MkV3 (r * cos u) hy (r * sin u)) na
                       c = MkVertex (MkV3 (r * cos u') hy (r * sin u')) nb
                       d = MkVertex (MkV3 (r * cos u') (-hy) (r * sin u')) nb
                    in [(a, b, c), (a, c, d)]
      up = MkV3 0.0 1.0 0.0
      down = MkV3 0.0 (-1.0) 0.0
      caps = \i => let u = cast i * du
                       u' = cast (i + 1) * du
                       pt = \uu, yy => MkV3 (r * cos uu) yy (r * sin uu)
                    in [ ( MkVertex (MkV3 0.0 hy 0.0) up
                         , MkVertex (pt u' hy) up
                         , MkVertex (pt u hy) up )
                       , ( MkVertex (MkV3 0.0 (-hy) 0.0) down
                         , MkVertex (pt u (-hy)) down
                         , MkVertex (pt u' (-hy)) down ) ]
   in concatMap (\i => side i ++ caps i) (range 0 (n - 1))

||| A cone around +y: base at -height/2, apex at +height/2, with a base cap.
public export
cone : (radius, height : Double) -> (segments : Int) -> MeshData
cone r h segments =
  let n = max 3 segments
      du = tau / cast n
      hy = h * 0.5
      len = sqrt (h * h + r * r)
      slant = \u => normalize3 (MkV3 (cos u * h) r (sin u * h))
      apex = MkV3 0.0 hy 0.0
      base = \u => MkV3 (r * cos u) (-hy) (r * sin u)
      down = MkV3 0.0 (-1.0) 0.0
      face = \i => let u = cast i * du
                       u' = cast (i + 1) * du
                    in [ ( MkVertex apex (slant ((u + u') * 0.5))
                         , MkVertex (base u') (slant u')
                         , MkVertex (base u) (slant u) )
                       , ( MkVertex (MkV3 0.0 (-hy) 0.0) down
                         , MkVertex (base u) down
                         , MkVertex (base u') down ) ]
   in concatMap face (range 0 (n - 1))

||| A filled circle in the XY plane facing +z.
public export
circle : (radius : Double) -> (segments : Int) -> MeshData
circle r segments =
  let n = max 3 segments
      du = tau / cast n
      fwd = MkV3 0.0 0.0 1.0
      pt = \u => MkV3 (r * cos u) (r * sin u) 0.0
      face = \i => let u = cast i * du
                       u' = cast (i + 1) * du
                    in ( MkVertex zero3 fwd
                       , MkVertex (pt u) fwd
                       , MkVertex (pt u') fwd )
   in map face (range 0 (n - 1))

--------------------------------------------------------------------------------
-- Upload

||| Flatten a triangle list into the buffer a renderer's `createMesh` takes:
||| position and normal, six floats a vertex, eighteen a triangle. One bounds
||| test per triangle rather than per component: the offsets inside a triangle
||| are literals, so they are proofs.
export
uploadMesh : MeshData -> IO (VertBuf Offler.Gfx.Layout.meshFloats)
uploadMesh mesh = do
  buf <- newVerts {stride = meshFloats} (cast (length mesh) * 3)
  go buf.arr 0 mesh
  pure buf
  where
    vert : F32Array cap -> At cap 6 -> Vertex -> IO ()
    vert arr o (MkVertex p n) = do
      poke arr (sub 0 o) p.vx; poke arr (sub 1 o) p.vy; poke arr (sub 2 o) p.vz
      poke arr (sub 3 o) n.vx; poke arr (sub 4 o) n.vy; poke arr (sub 5 o) n.vz

    tri : F32Array cap -> At cap 18 -> Tri -> IO ()
    tri arr o (a, b, c) = do
      vert arr (sub 0 o) a
      vert arr (sub 6 o) b
      vert arr (sub 12 o) c

    go : F32Array cap -> Int -> MeshData -> IO ()
    go arr _ [] = pure ()
    go arr i (t :: rest) = case window {w = 18} arr i of
      Nothing => pure ()   -- would overrun: stop rather than write past the end
      Just o => tri arr o t >> go arr (i + 18) rest

--------------------------------------------------------------------------------
-- Lines

||| World-space line segments, four padded floats a vertex so two vertices
||| fill one `poke16`, into the buffer `setLines` takes.
export
uploadLines : List (V3, V3) -> IO (VertBuf Offler.Gfx.Layout.lineFloats)
uploadLines segs = do
  buf <- newVerts {stride = lineFloats} (cast (length segs) * 2)
  go buf.arr 0 segs
  pure buf
  where
    go : F32Array cap -> Int -> List (V3, V3) -> IO ()
    go arr _ [] = pure ()
    go arr i ((a, b) :: rest) = case window {w = 8} arr i of
      Nothing => pure ()
      Just o => do
        poke4 arr (sub 0 o) a.vx a.vy a.vz 0.0
        poke4 arr (sub 4 o) b.vx b.vy b.vz 0.0
        go arr (i + 8) rest

||| Consecutive points joined into segments.
public export
polyline : List V3 -> List (V3, V3)
polyline [] = []
polyline (x :: xs) = go x xs
  where
    go : V3 -> List V3 -> List (V3, V3)
    go _ [] = []
    go prev (y :: ys) = (prev, y) :: go y ys

||| A closed loop of the given points.
public export
loop : List V3 -> List (V3, V3)
loop [] = []
loop (x :: xs) = polyline (x :: xs) ++ lastSeg x (x :: xs)
  where
    lastSeg : V3 -> List V3 -> List (V3, V3)
    lastSeg first [y] = [(y, first)]
    lastSeg first (_ :: ys) = lastSeg first ys
    lastSeg _ [] = []

||| A square grid in the XZ plane: `2n+1` lines each way, `spacing` apart.
public export
gridXZ : (n : Int) -> (spacing : Double) -> List (V3, V3)
gridXZ n spacing =
  let ext = cast n * spacing
   in concatMap (\i => let d = cast i * spacing
                        in [ (MkV3 (-ext) 0.0 d, MkV3 ext 0.0 d)
                           , (MkV3 d 0.0 (-ext), MkV3 d 0.0 ext) ])
                (range (-n) n)
