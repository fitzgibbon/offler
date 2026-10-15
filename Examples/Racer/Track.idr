||| The circuit: a figure of eight, a closed Catmull-Rom loop through
||| eighteen control points, sampled at `samples` points. One road crosses
||| the other at the origin, and a ramp before the crossover throws the
||| cars on that road over the traffic on the other. Everything about the
||| track -- the road, kerb and ramp meshes, the tyre walls, the trees that
||| keep off it, where a car is, which way the road goes there and how high
||| it is -- is a pure function of the curve.
module Examples.Racer.Track

import Data.List
import Offler.Color
import Offler.Math
import Offler.Mesh

%default covering

||| Half the road's width.
public export
halfWidth : Double
halfWidth = 4.6

||| How far from the centre line a car's centre may go before the tyre wall
||| stops it.
public export
wallReach : Double
wallReach = halfWidth + 1.4

controls : List V3
controls = map (\(x, z) => v3 (x * 1.35) 0.0 (z * 1.35))
  [ (34.0, 28.0), (55.0, 28.0), (68.0, 12.0), (66.0, -10.0), (50.0, -24.0)
  , (32.0, -22.0), (16.0, -16.0), (0.0, 0.0), (-16.0, 16.0), (-32.0, 22.0)
  , (-50.0, 24.0), (-66.0, 10.0), (-68.0, -12.0), (-55.0, -28.0), (-34.0, -28.0)
  , (-16.0, -16.0), (0.0, 0.0), (16.0, 16.0) ]

controlCount : Int
controlCount = 18

perControl : Int
perControl = 20

||| Samples around the loop: the unit of track position.
public export
samples : Int
samples = controlCount * perControl

||| Where each road passes the crossover: the flat one, and the one that
||| jumps it. Both are control points at the origin.
public export
flatCross, jumpCross : Int
flatCross = 7 * perControl
jumpCross = 16 * perControl

||| The ramp's lip, clear of the flat road's walls, and its foot.
lipAt, rampFoot : Double
lipAt = cast jumpCross - 5.0
rampFoot = lipAt - 12.0

||| The ramp's height at its lip.
public export
rampTop : Double
rampTop = 2.6

||| Wrap into `[0, n)`, negative inputs included.
export
wrapI : Int -> Int -> Int
wrapI n i = let r = i `mod` n in if r < 0 then r + n else r

nth : Int -> V3
nth i = go (wrapI controlCount i) controls
  where
    go : Int -> List V3 -> V3
    go _ [] = zero3
    go k (p :: ps) = if k <= 0 then p else go (k - 1) ps

lin : List (Double, V3) -> V3
lin = foldl (\acc, (k, p) => add3 acc (scale3 k p)) zero3

||| Position and unit tangent at track position `s`, in samples, wrapping.
export
along : Double -> (V3, V3)
along s =
  let u = s * cast controlCount / cast samples
      k = floor u
      t = u - k
      i = the Int (cast k)
      p0 = nth (i - 1); p1 = nth i; p2 = nth (i + 1); p3 = nth (i + 2)
      t2 = t * t; t3 = t2 * t
      pos = scale3 0.5 (lin [ (2.0, p1), (t, sub3 p2 p0)
                            , (t2, lin [(2.0, p0), (-5.0, p1), (4.0, p2), (-1.0, p3)])
                            , (t3, lin [(-1.0, p0), (3.0, p1), (-3.0, p2), (1.0, p3)]) ])
      d = lin [ (1.0, sub3 p2 p0)
              , (2.0 * t, lin [(2.0, p0), (-5.0, p1), (4.0, p2), (-1.0, p3)])
              , (3.0 * t2, lin [(-1.0, p0), (3.0, p1), (-3.0, p2), (1.0, p3)]) ]
   in (pos, normalize3 d)

||| The sideways unit vector at a tangent: positive lateral offsets lie this
||| way.
export
sideOf : V3 -> V3
sideOf t = v3 (-t.vz) 0.0 t.vx

||| Heading angle of a direction in the ground plane: forward is
||| `(cos h, 0, sin h)`.
export
headingOf : V3 -> Double
headingOf d = atan2 d.vz d.vx

||| Fold `[a .. b]` without building the range.
foldRange : Int -> Int -> acc -> (acc -> Int -> acc) -> acc
foldRange a b z f = go a z
  where
    go : Int -> acc -> acc
    go i acc = if i > b then acc else go (i + 1) (f acc i)

||| The nearest sample to `p`, searched a window around `hint`: the
||| sample, its signed lateral offset and its distance.
export
nearest : (hint : Int) -> V3 -> (Int, Double)
nearest hint p =
  let (best, _) = foldRange (hint - 8) (hint + 14) (hint, 1.0e9) $ \(b, bd), i =>
        let (c, _) = along (cast i)
            d = length3 (sub3 p c)
         in if d < bd then (i, d) else (b, bd)
      k = wrapI samples best
      (c, t) = along (cast k)
   in (k, dot3 (sub3 p c) (sideOf t))

||| The road's height at track position `s`: the ramp, or the ground.
export
rampHeight : Double -> Double
rampHeight s = if s >= rampFoot && s <= lipAt then rampTop * (s - rampFoot) / (lipAt - rampFoot) else 0.0

||| The road's height under `p`, beside sample `seg`.
export
surface : (seg : Int) -> V3 -> Double
surface seg p =
  let (c, t) = along (cast seg)
      (c1, _) = along (cast seg + 1.0)
      step = max 0.01 (length3 (sub3 c1 c))
   in rampHeight (cast seg + dot3 (sub3 p c) t / step)

||| Whether `p`, near sample `i`, lies within `r` of the other road's centre
||| line: only ever true by the crossover.
crossedBy : Int -> Double -> V3 -> Bool
crossedBy i r p =
  let other = if abs (i - jumpCross) < 30 then Just flatCross
              else if abs (i - flatCross) < 30 then Just jumpCross else Nothing
   in case other of
        Nothing => False
        Just j => foldRange (j - 12) (j + 12) False $ \acc, k =>
                    acc || length3 (sub3 p (fst (along (cast k)))) < r

||| Samples of the jumping road that lie on the flat one: it is drawn over
||| them, unbroken.
underFlat : Int -> Bool
underFlat i = abs (i - jumpCross) < 30 && crossedBy i (halfWidth + 0.9) (fst (along (cast i)))

||| How sharply the road turns between two track positions, in radians.
export
turnBetween : Double -> Double -> Double
turnBetween a b =
  let (_, ta) = along a
      (_, tb) = along b
   in acos (max (-1.0) (min 1.0 (dot3 ta tb)))

--------------------------------------------------------------------------------
-- Geometry

||| A triangle facing up, whatever order its corners come in.
upTri : (V3, V2) -> (V3, V2) -> (V3, V2) -> Tri
upTri (a, ua) (b, ub) (c, uc) =
  let up = v3 0.0 1.0 0.0
      vx = \p, uv => MkVertex p up uv
   in if (cross3 (sub3 b a) (sub3 c a)).vy >= 0.0
        then (vx a ua, vx b ub, vx c uc)
        else (vx a ua, vx c uc, vx b ub)

||| A triangle facing `n`, whatever order its corners come in.
faceTri : V3 -> V3 -> V3 -> V3 -> Tri
faceTri n a b c =
  let vx = \p => MkVertex p n (MkV2 0.0 0.0)
   in if dot3 (cross3 (sub3 b a) (sub3 c a)) n >= 0.0
        then (vx a, vx b, vx c)
        else (vx a, vx c, vx b)

quad : V3 -> V3 -> V3 -> V3 -> V3 -> List Tri
quad n a b c d = [faceTri n a b c, faceTri n a c d]

||| The road's height at each end of the ribbon from sample `i`: up the
||| ramp, and flat from its lip on.
ends : Int -> (Double, Double)
ends i =
  let h0 = rampHeight (cast i)
      h1 = rampHeight (cast (i + 1))
   in if h0 > 0.0 && h1 <= 0.0 then (0.0, 0.0) else (h0, h1)

||| A ribbon between lateral offsets `inner` and `outer` from sample `i`
||| to `i + 1`, `y` above the road.
strip : Double -> Double -> Double -> Int -> List Tri
strip y inner outer i =
  let (p0, t0) = along (cast i)
      (p1, t1) = along (cast (i + 1))
      (h0, h1) = ends i
      at = \p, h, t, o => add3 (v3 p.vx (y + h) p.vz) (scale3 o (sideOf t))
      a = at p0 h0 t0 inner; b = at p0 h0 t0 outer
      c = at p1 h1 t1 outer; d = at p1 h1 t1 inner
      v0 = cast i / 4.0; v1 = cast (i + 1) / 4.0
   in [ upTri (a, MkV2 0.0 v0) (b, MkV2 1.0 v0) (c, MkV2 1.0 v1)
      , upTri (a, MkV2 0.0 v0) (c, MkV2 1.0 v1) (d, MkV2 0.0 v1) ]

collect : (Int -> List Tri) -> (Int -> Bool) -> List Tri
collect f keep = reverse (foldRange 0 (samples - 1) [] (\acc, i =>
  if keep i then reverseOnto acc (f i) else acc))

||| The tarmac.
export
roadTris : List Tri
roadTris = collect (strip 0.02 (-halfWidth) halfWidth) (not . underFlat)

||| Kerbs on both edges, alternating colour every two samples: `True`
||| gives the red ones, `False` the white.
export
kerbTris : Bool -> List Tri
kerbTris red =
  collect (\i => strip 0.04 halfWidth (halfWidth + 0.8) i
              ++ strip 0.04 (-halfWidth - 0.8) (-halfWidth) i)
          (\i => (i `div` 2) `mod` 2 == (if red then 0 else 1) && not (underFlat i))

||| One side of the ramp, from sample `i` to `i + 1`: `sgn` picks which.
rampSide : Double -> Int -> List Tri
rampSide sgn i =
  let (p0, t0) = along (cast i)
      (p1, t1) = along (cast (i + 1))
      n0 = scale3 sgn (sideOf t0); n1 = scale3 sgn (sideOf t1)
      e0 = add3 p0 (scale3 wallReach n0); e1 = add3 p1 (scale3 wallReach n1)
   in quad n0 e0 e1 (v3 e1.vx (rampHeight (cast (i + 1))) e1.vz) (v3 e0.vx (rampHeight (cast i)) e0.vz)

||| The ramp's concrete: a deck from wall to wall, its sides, and the face
||| under its lip.
export
rampTris : List Tri
rampTris =
  let foot = the Int (cast rampFoot)
      lip = the Int (cast lipAt)
      deck = collect (strip 0.0 (-wallReach) wallReach) (\i => i >= foot && i < lip)
      sides = collect (\i => rampSide 1.0 i ++ rampSide (-1.0) i) (\i => i >= foot && i < lip)
      (pl, tl) = along lipAt
      across = scale3 wallReach (sideOf tl)
      l0 = sub3 pl across; l1 = add3 pl across
      face = quad tl l0 l1 (v3 l1.vx rampTop l1.vz) (v3 l0.vx rampTop l0.vz)
   in deck ++ sides ++ face

||| The dashed centre line, as gizmo lines.
export
centreDashes : List (V3, V3, Color)
centreDashes = foldRange 0 (samples - 1) [] $ \acc, i =>
  if i `mod` 3 /= 0 || underFlat i || underFlat (i + 1) then acc else
    let (a, _) = along (cast i)
        (b, _) = along (cast i + 1.4)
        (h0, _) = ends i
        h1 = if h0 > 0.0 then rampHeight (cast i + 1.4) else 0.0
        lift = \p, h => v3 p.vx (h + 0.06) p.vz
     in (lift a h0, lift b h1, rgb 0.9 0.9 0.85) :: acc

||| Tyre stacks along both walls, open where the other road crosses:
||| position, and whether it is a red one.
export
tyres : List (V3, Bool)
tyres = foldRange 0 (samples - 1) [] $ \acc, i =>
  if i `mod` 2 /= 0 then acc else
    let (p, t) = along (cast i)
        n = sideOf t
        red = (i `div` 2) `mod` 2 == 0
     in put i (add3 p (scale3 (wallReach + 0.7) n)) red
          (put i (add3 p (scale3 (-(wallReach + 0.7)) n)) (not red) acc)
  where
    put : Int -> V3 -> Bool -> List (V3, Bool) -> List (V3, Bool)
    put i q red rest = if crossedBy i (wallReach + 0.4) q then rest else (q, red) :: rest

hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

||| Trees scattered over the infield and outfield, none within reach of the
||| road: position and size.
export
trees : List (V3, Double)
trees = foldRange 0 299 [] $ \acc, k =>
  let p = v3 (-130.0 + 260.0 * hashUnit (k * 7 + 1)) 0.0 (-65.0 + 130.0 * hashUnit (k * 13 + 5))
      clear = foldRange 0 (samples `div` 4 - 1) True $ \ok, i =>
                ok && length3 (sub3 p (fst (along (cast (i * 4))))) > wallReach + 6.0
   in if clear then (p, 0.8 + 1.0 * hashUnit (k * 3 + 2)) :: acc else acc
