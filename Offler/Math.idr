||| Vectors, quaternions and 4x4 matrices.
|||
||| `Mat4` is a flat record rather than a `List Double`. That matters once a
||| scene has thousands of bodies: list indexing is O(n), so every multiply
||| would walk about a thousand cons cells and allocate sixteen more. A record
||| is O(1) per field and one allocation per matrix.
module Offler.Math

import Offler.Gfx.Array

%default total

--------------------------------------------------------------------------------
-- Vectors

public export
record V2 where
  constructor MkV2
  vx, vy : Double

public export
record V3 where
  constructor MkV3
  vx, vy, vz : Double

public export
v3 : Double -> Double -> Double -> V3
v3 = MkV3

public export
zero3 : V3
zero3 = MkV3 0.0 0.0 0.0

public export
one3 : V3
one3 = MkV3 1.0 1.0 1.0

public export
scale3 : Double -> V3 -> V3
scale3 k (MkV3 x y z) = MkV3 (k * x) (k * y) (k * z)

public export
add3 : V3 -> V3 -> V3
add3 (MkV3 a b c) (MkV3 d e f) = MkV3 (a + d) (b + e) (c + f)

public export
sub3 : V3 -> V3 -> V3
sub3 (MkV3 a b c) (MkV3 d e f) = MkV3 (a - d) (b - e) (c - f)

public export
dot3 : V3 -> V3 -> Double
dot3 (MkV3 a b c) (MkV3 d e f) = a * d + b * e + c * f

public export
cross3 : V3 -> V3 -> V3
cross3 (MkV3 ax ay az) (MkV3 bx by bz) =
  MkV3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

public export
length3 : V3 -> Double
length3 v = sqrt (dot3 v v)

||| Normalise, leaving the zero vector alone rather than dividing by it.
public export
normalize3 : V3 -> V3
normalize3 v =
  let len = length3 v
   in if len == 0.0 then v else scale3 (1.0 / len) v

||| Project onto the unit sphere. The same as `normalize3`, named for what the
||| mesh builders use it for.
public export
onSphere : V3 -> V3
onSphere = normalize3

public export
midpoint : V3 -> V3 -> V3
midpoint a b = onSphere (scale3 0.5 (add3 a b))

--------------------------------------------------------------------------------
-- Quaternions

||| A rotation. `qw` is the scalar part.
public export
record Quat where
  constructor MkQuat
  qx, qy, qz, qw : Double

public export
qIdentity : Quat
qIdentity = MkQuat 0.0 0.0 0.0 1.0

||| Rotation of `angle` radians about `axis`, which need not be unit length.
public export
axisAngle : (axis : V3) -> (angle : Double) -> Quat
axisAngle axis angle =
  let MkV3 x y z = normalize3 axis
      h = angle * 0.5
      s = sin h
   in MkQuat (x * s) (y * s) (z * s) (cos h)

export infixl 7 `qmul`

||| `qmul a b` applies `b` first, then `a`, matching `mmul`.
public export
qmul : Quat -> Quat -> Quat
qmul (MkQuat ax ay az aw) (MkQuat bx by bz bw) =
  MkQuat (aw * bx + ax * bw + ay * bz - az * by)
         (aw * by - ax * bz + ay * bw + az * bx)
         (aw * bz + ax * by - ay * bx + az * bw)
         (aw * bw - ax * bx - ay * by - az * bz)

public export
qNormalize : Quat -> Quat
qNormalize q@(MkQuat x y z w) =
  let len = sqrt (x * x + y * y + z * z + w * w)
   in if len == 0.0 then qIdentity
      else MkQuat (x / len) (y / len) (z / len) (w / len)

||| The inverse of a unit quaternion.
public export
qConjugate : Quat -> Quat
qConjugate (MkQuat x y z w) = MkQuat (-x) (-y) (-z) w

||| Rotate a vector by a unit quaternion.
public export
qRotate : Quat -> V3 -> V3
qRotate (MkQuat qx qy qz qw) v =
  let u = MkV3 qx qy qz
      t = scale3 2.0 (cross3 u v)
   in add3 v (add3 (scale3 qw t) (cross3 u t))

||| Euler angles applied Z, then X, then Y -- bevy's `EulerRot::YXZ` default.
public export
fromEulerYXZ : (yaw, pitch, roll : Double) -> Quat
fromEulerYXZ yaw pitch roll =
  axisAngle (MkV3 0.0 1.0 0.0) yaw
    `qmul` axisAngle (MkV3 1.0 0.0 0.0) pitch
    `qmul` axisAngle (MkV3 0.0 0.0 1.0) roll

--------------------------------------------------------------------------------
-- Matrices

||| Column-major, so element (row r, column c) is field `m(c*4+r)` -- the
||| layout both `uniformMatrix4fv` and WGSL expect.
public export
record Mat4 where
  constructor MkMat4
  m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15 : Double

export infixl 7 `mmul`

||| `a `mmul` b` applies b first, then a.
public export
mmul : Mat4 -> Mat4 -> Mat4
mmul a b =
  MkMat4
    (a.m0 * b.m0 + a.m4 * b.m1 + a.m8 * b.m2 + a.m12 * b.m3)
    (a.m1 * b.m0 + a.m5 * b.m1 + a.m9 * b.m2 + a.m13 * b.m3)
    (a.m2 * b.m0 + a.m6 * b.m1 + a.m10 * b.m2 + a.m14 * b.m3)
    (a.m3 * b.m0 + a.m7 * b.m1 + a.m11 * b.m2 + a.m15 * b.m3)
    (a.m0 * b.m4 + a.m4 * b.m5 + a.m8 * b.m6 + a.m12 * b.m7)
    (a.m1 * b.m4 + a.m5 * b.m5 + a.m9 * b.m6 + a.m13 * b.m7)
    (a.m2 * b.m4 + a.m6 * b.m5 + a.m10 * b.m6 + a.m14 * b.m7)
    (a.m3 * b.m4 + a.m7 * b.m5 + a.m11 * b.m6 + a.m15 * b.m7)
    (a.m0 * b.m8 + a.m4 * b.m9 + a.m8 * b.m10 + a.m12 * b.m11)
    (a.m1 * b.m8 + a.m5 * b.m9 + a.m9 * b.m10 + a.m13 * b.m11)
    (a.m2 * b.m8 + a.m6 * b.m9 + a.m10 * b.m10 + a.m14 * b.m11)
    (a.m3 * b.m8 + a.m7 * b.m9 + a.m11 * b.m10 + a.m15 * b.m11)
    (a.m0 * b.m12 + a.m4 * b.m13 + a.m8 * b.m14 + a.m12 * b.m15)
    (a.m1 * b.m12 + a.m5 * b.m13 + a.m9 * b.m14 + a.m13 * b.m15)
    (a.m2 * b.m12 + a.m6 * b.m13 + a.m10 * b.m14 + a.m14 * b.m15)
    (a.m3 * b.m12 + a.m7 * b.m13 + a.m11 * b.m14 + a.m15 * b.m15)

public export
identity : Mat4
identity =
  MkMat4 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0

||| OpenGL-convention perspective: clip z in [-1, 1]. The renderers rewrite the
||| z row for the [0, 1] APIs -- see `correctClipZ`.
public export
perspective : (fovy, aspectRatio, near, far : Double) -> Mat4
perspective fovy aspectRatio near far =
  let f = 1.0 / tan (fovy / 2.0)
      nf = 1.0 / (near - far)
   in MkMat4 (f / aspectRatio) 0.0 0.0 0.0
             0.0 f 0.0 0.0
             0.0 0.0 ((far + near) * nf) (-1.0)
             0.0 0.0 (2.0 * far * near * nf) 0.0

||| Symmetric orthographic projection, given the vertical half-extent. Clip z
||| in [-1, 1], corrected the same way perspective is.
public export
orthographic : (halfHeight, aspectRatio, near, far : Double) -> Mat4
orthographic halfHeight aspectRatio near far =
  let halfWidth = halfHeight * aspectRatio
      nf = 1.0 / (near - far)
   in MkMat4 (1.0 / halfWidth) 0.0 0.0 0.0
             0.0 (1.0 / halfHeight) 0.0 0.0
             0.0 0.0 (2.0 * nf) 0.0
             0.0 0.0 ((far + near) * nf) 1.0

public export
translate : (x, y, z : Double) -> Mat4
translate x y z =
  MkMat4 1.0 0.0 0.0 0.0  0.0 1.0 0.0 0.0  0.0 0.0 1.0 0.0  x y z 1.0

public export
scaleM : Double -> Mat4
scaleM k = MkMat4 k 0.0 0.0 0.0  0.0 k 0.0 0.0  0.0 0.0 k 0.0  0.0 0.0 0.0 1.0

public export
scaleXYZ : (x, y, z : Double) -> Mat4
scaleXYZ x y z =
  MkMat4 x 0.0 0.0 0.0  0.0 y 0.0 0.0  0.0 0.0 z 0.0  0.0 0.0 0.0 1.0

public export
rotateX : Double -> Mat4
rotateX a =
  let c = cos a
      s = sin a
   in MkMat4 1.0 0.0 0.0 0.0  0.0 c s 0.0  0.0 (-s) c 0.0  0.0 0.0 0.0 1.0

public export
rotateY : Double -> Mat4
rotateY a =
  let c = cos a
      s = sin a
   in MkMat4 c 0.0 (-s) 0.0  0.0 1.0 0.0 0.0  s 0.0 c 0.0  0.0 0.0 0.0 1.0

public export
rotateZ : Double -> Mat4
rotateZ a =
  let c = cos a
      s = sin a
   in MkMat4 c s 0.0 0.0  (-s) c 0.0 0.0  0.0 0.0 1.0 0.0  0.0 0.0 0.0 1.0

||| The rotation matrix of a unit quaternion.
public export
matOfQuat : Quat -> Mat4
matOfQuat (MkQuat x y z w) =
  let x2 = x + x; y2 = y + y; z2 = z + z
      xx = x * x2; xy = x * y2; xz = x * z2
      yy = y * y2; yz = y * z2; zz = z * z2
      wx = w * x2; wy = w * y2; wz = w * z2
   in MkMat4 (1.0 - (yy + zz)) (xy + wz) (xz - wy) 0.0
             (xy - wz) (1.0 - (xx + zz)) (yz + wx) 0.0
             (xz + wy) (yz - wx) (1.0 - (xx + yy)) 0.0
             0.0 0.0 0.0 1.0

||| A view matrix looking from `eye` towards `target`, `up` steadying the roll.
public export
lookAt : (eye, target, up : V3) -> Mat4
lookAt eye target up =
  let f = normalize3 (sub3 target eye)          -- forward
      s = normalize3 (cross3 f up)              -- right
      u = cross3 s f                            -- true up
   in MkMat4 s.vx u.vx (-f.vx) 0.0
             s.vy u.vy (-f.vy) 0.0
             s.vz u.vz (-f.vz) 0.0
             (-(dot3 s eye)) (-(dot3 u eye)) (dot3 f eye) 1.0

public export
pokeMat : F32Array cap -> At cap 16 -> Mat4 -> IO ()
pokeMat a o m = poke16 a o m.m0 m.m1 m.m2 m.m3 m.m4 m.m5 m.m6 m.m7 m.m8 m.m9 m.m10 m.m11 m.m12 m.m13 m.m14 m.m15

||| Rewrite the z row of a column-major projection so clip-space z lands in
||| [0,1] rather than [-1,1]: `row2' = 0.5*row2 + 0.5*row3`.
|||
||| WebGL wants [-1,1] and WebGPU wants [0,1]. Without this the scene falls
||| behind the near plane and nothing draws, with no validation error to say so.
||| Both WebGPU-flavoured backends need it, browser and native alike, so it lives
||| here rather than in either one.
||| The four columns are written out rather than looped over: every offset is
||| then a literal inside the sixteen floats the caller already established, so
||| the bounds are proofs and there is no loop counter to get wrong. It is also
||| total, which the loop was not.
public export
correctClipZ : F32Array cap -> At cap 16 -> IO ()
correctClipZ a o = do
  fix (sub 2 o) (sub 3 o)
  fix (sub 6 o) (sub 7 o)
  fix (sub 10 o) (sub 11 o)
  fix (sub 14 o) (sub 15 o)
  where
    fix : At cap 1 -> At cap 1 -> IO ()
    fix zAt wAt = do
      z <- peek a zAt
      w <- peek a wAt
      poke a zAt (0.5 * z + 0.5 * w)

--------------------------------------------------------------------------------
-- Constants

-- `pi` is the Prelude's; shadowing it would make every bare `pi` ambiguous.
public export
tau : Double
tau = 6.28318530717958647692

||| The two-argument arctangent the Prelude lacks: the angle of (x, y) in
||| (-pi, pi].
public export
atan2 : (y : Double) -> (x : Double) -> Double
atan2 y x =
  if x > 0.0 then atan (y / x)
  else if x < 0.0 then (if y >= 0.0 then atan (y / x) + pi else atan (y / x) - pi)
  else if y > 0.0 then pi / 2.0
  else if y < 0.0 then -(pi / 2.0)
  else 0.0

--------------------------------------------------------------------------------
-- Iteration

||| Ascending `[lo .. hi]`, and empty when `hi < lo`.
|||
||| Not a drop-in for the Prelude's `rangeFromTo`, which counts *down* when
||| inverted: `[1 .. 0]` is `[1, 0]`, where `range 1 0` is empty. Empty is
||| what a count wants.
|||
||| Tail recursive, which `rangeFromTo` is not: the Prelude builds ranges with
||| `takeUntil`, which conses and so spends a stack frame per element,
||| overflowing V8 around 5700 elements though Chez and SpiderMonkey survive
||| it. `takeUntil` has no `%transform` to an accumulator-passing form, so
||| this reproduces by hand what the Prelude does for `map` and `filter`.
public export
range : Int -> Int -> List Int
range lo hi = go (cast (hi - lo + 1)) lo Lin
  where
    -- Structural recursion on a Nat fuel, so this is total; the accumulator
    -- keeps it a self tail call, so it trampolines on the JS backend.
    go : Nat -> Int -> SnocList Int -> List Int
    go Z _ acc = acc <>> []
    go (S k) i acc = go k (i + 1) (acc :< i)
