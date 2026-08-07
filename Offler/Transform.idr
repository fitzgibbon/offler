||| Position, rotation and scale, composed in bevy's order: scale first, then
||| rotate, then translate.
module Offler.Transform

import Offler.Math

%default total

public export
record Transform where
  constructor MkTransform
  translation : V3
  rotation : Quat
  scale : V3

public export
neutral : Transform
neutral = MkTransform zero3 qIdentity one3

public export
at : V3 -> Transform
at p = MkTransform p qIdentity one3

public export
withRotation : Quat -> Transform -> Transform
withRotation q t = { rotation := q } t

public export
withScale : V3 -> Transform -> Transform
withScale s t = { scale := s } t

public export
uniformScale : Double -> Transform -> Transform
uniformScale k t = { scale := MkV3 k k k } t

||| The model matrix: translate . rotate . scale.
public export
matOf : Transform -> Mat4
matOf (MkTransform p q s) =
  translate p.vx p.vy p.vz `mmul` matOfQuat q `mmul` scaleXYZ s.vx s.vy s.vz

||| The inverse of a rigid transform with uniform-or-no scale interest: the
||| view matrix of a camera posed by this transform. Computed directly rather
||| than by a general 4x4 inverse: transpose the rotation, invert the scale,
||| and carry the translation through them.
public export
viewOf : Transform -> Mat4
viewOf (MkTransform p q s) =
  scaleXYZ (inv s.vx) (inv s.vy) (inv s.vz)
    `mmul` matOfQuat (qConjugate q)
    `mmul` translate (-p.vx) (-p.vy) (-p.vz)
  where
    inv : Double -> Double
    inv x = if x == 0.0 then 0.0 else 1.0 / x

||| Pose a transform to look from its translation towards `target`.
public export
lookingAt : (target : V3) -> (up : V3) -> Transform -> Transform
lookingAt target up t =
  let f = normalize3 (sub3 target t.translation)   -- forward, -z
      r = normalize3 (cross3 f up)                 -- right, +x
      u = cross3 r f                               -- true up, +y
      -- Rotation matrix with columns (r, u, -f) as a quaternion.
      m00 = r.vx; m01 = u.vx; m02 = -f.vx
      m10 = r.vy; m11 = u.vy; m12 = -f.vy
      m20 = r.vz; m21 = u.vz; m22 = -f.vz
      trace = m00 + m11 + m22
      q = if trace > 0.0
            then let w = sqrt (1.0 + trace) * 0.5
                     k = 0.25 / w
                  in MkQuat ((m21 - m12) * k) ((m02 - m20) * k) ((m10 - m01) * k) w
            else if m00 >= m11 && m00 >= m22
            then let x = sqrt (max 0.0 (1.0 + m00 - m11 - m22)) * 0.5
                     k = 0.25 / x
                  in MkQuat x ((m01 + m10) * k) ((m02 + m20) * k) ((m21 - m12) * k)
            else if m11 >= m22
            then let y = sqrt (max 0.0 (1.0 - m00 + m11 - m22)) * 0.5
                     k = 0.25 / y
                  in MkQuat ((m01 + m10) * k) y ((m12 + m21) * k) ((m02 - m20) * k)
            else let z = sqrt (max 0.0 (1.0 - m00 - m11 + m22)) * 0.5
                     k = 0.25 / z
                  in MkQuat ((m02 + m20) * k) ((m12 + m21) * k) z ((m10 - m01) * k)
   in { rotation := qNormalize q } t
