||| An isometric racer in the spirit of Supercars 2: eight cars on one
||| figure-of-eight circuit with a jump over the crossover, an endless race,
||| and front-firing missiles that spin a car out. You drive the red car
||| against seven CPU drivers.
|||
||| It is the ECS with everything in it. Each car is an entity hierarchy --
||| body, cabin and wheels under a root that `propagate` places -- driven by
||| `Pad` controls that the input system writes for you and the AI writes
||| for the others. Physics, collisions, lap timing, missiles and sparks
||| are systems in one `Schedule`. Input arrives as `Events Event` and sound
||| cues leave as `Events Sfx`; the held keys, the camera and the standings
||| are resources. Missiles, sparks, tyre walls and trees are instanced
||| batches drawn straight from their columns; the walls and trees never
||| change, and change detection uploads them exactly once. Click or tap a
||| car to follow it -- ray-picked through its `Pickable` -- and the
||| Hologram material from the Custom example hangs over the start line.
module Examples.Racer.Scene

import Data.IORef
import Data.List
import Data.Vect
import Examples.Assets
import Examples.Custom.Scene
import Examples.Racer.Sound
import Examples.Racer.Track
import Examples.Util
import Offler.Camera
import Offler.Color
import Offler.Ecs
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Gizmos
import Offler.Light
import Offler.Material
import Offler.Math
import Offler.Mesh
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

-- The world has thirty component types, and proof search walks the
-- list once per membership proof and once per store: past the default
-- depth.
%auto_implicit_depth 128

--------------------------------------------------------------------------------
-- Components

||| A car's motion: forward is `(cos heading, 0, sin heading)`, and height
||| and climb are `pos.vy` and `vel.vy`.
record Kin where
  constructor MkKin
  pos : V3
  heading : Double
  vel : V3

||| What a car is being asked to do this frame, by a person or the AI.
record Pad where
  constructor MkPad
  steer, throttle, brake : Double
  fire : Bool

||| Where a car is in the race. `crossings` counts start-line crossings
||| (-1 on the grid); a lap counts only if the car passed halfway first.
record Racer where
  constructor MkRacer
  number : Int
  seg : Int
  crossings : Int
  armed : Bool
  lapStart, lastLap, bestLap : Double

record Cpu where
  constructor MkCpu
  skill : Double
  lane : Double

||| The car you drive.
data Human = MkHuman

record Weapon where
  constructor MkWeapon
  ammo : Int
  cooldown : Double

||| Seconds left spinning after a hit.
record Spin where
  constructor MkSpin
  left : Double

record Missile where
  constructor MkMissile
  owner : Entity
  mseg : Int
  mpos : V3
  mvel : V3
  mlife : Double

-- Particles: packed, since there are hundreds.

record Pos where
  constructor MkPos
  pos : V3

record Vel where
  constructor MkVel
  vel : V3

record Life where
  constructor MkLife
  age, span : Double

record Hue where
  constructor MkHue
  hue, sat, light : Double

Columnar Pos where
  width = 3
  storeCol a o (MkPos p) = do
    poke a (field 0 o) p.vx; poke a (field 1 o) p.vy; poke a (field 2 o) p.vz
  loadCol a o = do
    x <- peek a (field 0 o); y <- peek a (field 1 o); z <- peek a (field 2 o)
    pure (MkPos (MkV3 x y z))

Columnar Vel where
  width = 3
  storeCol a o (MkVel v) = do
    poke a (field 0 o) v.vx; poke a (field 1 o) v.vy; poke a (field 2 o) v.vz
  loadCol a o = do
    x <- peek a (field 0 o); y <- peek a (field 1 o); z <- peek a (field 2 o)
    pure (MkVel (MkV3 x y z))

Columnar Life where
  width = 2
  storeCol a o (MkLife ag sp) = poke a (field 0 o) ag >> poke a (field 1 o) sp
  loadCol a o = [| MkLife (peek a (field 0 o)) (peek a (field 1 o)) |]

Columnar Hue where
  width = 3
  storeCol a o (MkHue h s l) = do
    poke a (field 0 o) h; poke a (field 1 o) s; poke a (field 2 o) l
  loadCol a o = [| MkHue (peek a (field 0 o)) (peek a (field 1 o)) (peek a (field 2 o)) |]

||| Instanced batches.
data Spark : Type where
data Rocket : Type where
data Tyre : Type where
data Pine : Type where
data Shade : Type where

Component Kin where kind = Boxed
Component Pad where kind = Boxed
Component Racer where kind = Boxed
Component Cpu where kind = Boxed
Component Human where kind = Marker MkHuman
Component Weapon where kind = Boxed
Component Spin where kind = Boxed
Component Missile where kind = Boxed
Component Pos where kind = Packed
Component Vel where kind = Packed
Component Life where kind = Packed
Component Hue where kind = Packed

--------------------------------------------------------------------------------
-- Resources and events

||| Keys held down now.
record Keys where
  constructor MkKeys
  held : List String

||| Fingers down now: id and position.
record Touches where
  constructor MkTouches
  fingers : List (Int, Double, Double)

||| The car the camera follows; `Nothing` is yours.
record Follow where
  constructor MkFollow
  car : Maybe Entity

||| Where the camera looks.
record View where
  constructor MkView
  focus : V3

||| Your place: position and field size.
record Standings where
  constructor MkStandings
  place, field : Int

||| Seconds until the start; the race starts at zero, and "GO" shows for a
||| second after.
record Countdown where
  constructor MkCountdown
  left : Double

Resource Countdown where initial = MkCountdown 3.5
Resource Keys where initial = MkKeys []
Resource Touches where initial = MkTouches []
Resource Follow where initial = MkFollow Nothing
Resource View where initial = MkView zero3
Resource Standings where initial = MkStandings 1 8

||| Sound cues, positioned where they happen.
public export
data Sfx = Fire V3 | Boom V3 | Lap | Scrape V3

public export
W : List Type
W = [ Transform, GlobalTransform, ChildOf, Renderable, Pickable
    , Kin, Pad, Racer, Cpu, Human, Weapon, Spin, Missile
    , Pos, Vel, Life, Hue
    , Instance Spark, Instance Rocket, Instance Tyre, Instance Pine, Instance Shade
    , Countdown, Keys, Touches, Follow, View, Standings, Events Event, Events Sfx ]

maxEntities : Int
maxEntities = 4096

carCount : Int
carCount = 8

maxAmmo : Int
maxAmmo = 5

topSpeed : Double
topSpeed = 30.0

gravity : Double
gravity = 18.0

--------------------------------------------------------------------------------
-- Small helpers

clamp : Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

forwardOf : Double -> V3
forwardOf h = v3 (cos h) 0.0 (sin h)

rightOf : Double -> V3
rightOf h = v3 (-(sin h)) 0.0 (cos h)

flat : V3 -> V3
flat v = v3 v.vx 0.0 v.vz

||| An angle folded into `(-pi, pi]`.
wrapAngle : Double -> Double
wrapAngle a =
  let k = floor ((a + pi) / tau)
   in a - k * tau

hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

||| Track progress for the standings: crossings, then position on the lap.
progress : Racer -> Int
progress r = r.crossings * samples + r.seg

||| m:ss.t
lapTime : Double -> String
lapTime t =
  if t <= 0.0 then "--" else
    let tenths = the Int (cast (t * 10.0 + 0.5))
        m = tenths `div` 600
        s = (tenths `div` 10) `mod` 60
        d = tenths `mod` 10
     in show m ++ ":" ++ (if s < 10 then "0" else "") ++ show s ++ "." ++ show d

isoOffset : V3
isoOffset = v3 22.0 24.0 22.0

camera : V3 -> Camera
camera focus = withClearColor (srgb 0.2 0.42 0.15) $ orthographicCamera 15.0
  (lookingAt focus (v3 0.0 1.0 0.0) (at (add3 focus isoOffset)))

||| Screen-right in the ground plane, for panning sounds.
screenRight : V3
screenRight = normalize3 (v3 1.0 0.0 (-1.0))

--------------------------------------------------------------------------------
-- Systems

||| A spark burst: `n` particles from `p`, coloured around `hue`.
burst : World W -> V3 -> Int -> Double -> Double -> Int -> IO ()
burst w p n hue light seed =
  if n <= 0 then pure () else do
    let h = hashUnit (seed * 7 + n)
        a = tau * hashUnit (seed * 13 + n * 3)
        up = 3.0 + 7.0 * hashUnit (seed * 17 + n * 5)
        sp = 3.0 + 9.0 * h
        v = v3 (cos a * sp) up (sin a * sp)
    _ <- spawn w (MkPos (add3 p (v3 0.0 0.6 0.0)), MkVel v,
                  MkLife 0.0 (0.4 + 0.8 * h), MkHue (hue + 0.08 * (h - 0.5)) 0.9 light)
    burst w p (n - 1) hue light seed

leftKeys, rightKeys, gasKeys, brakeKeys : List String
leftKeys = ["ArrowLeft", "a", "A"]
rightKeys = ["ArrowRight", "d", "D"]
gasKeys = ["ArrowUp", "w", "W"]
brakeKeys = ["ArrowDown", "s", "S"]

||| 1 if any of `keys` is held, else 0.
heldAmount : List String -> List String -> Double
heldAmount held keys = if any (\k => elem k held) keys then 1.0 else 0.0

||| The start: lights count down, a tone on each, a higher one for go.
export
startSys : System W
startSys t w = do
  MkCountdown left <- resource {c = Countdown} w
  when (left > -1.0) $ do
    let left' = left - t.delta
    when (floor left' < floor left && left' > -0.5) $ sendEvent w Lap
    setResource w (MkCountdown left')

||| Whether the race has started.
racing : World W -> IO Bool
racing w = map (\(MkCountdown l) => l <= 0.0) (resource {c = Countdown} w)

||| Input: events update the held keys and fingers, edges fire missiles and
||| switch the camera, and the held state, fingers and first gamepad become
||| your car's `Pad`.
inputSys : Platform p => Audio au => p -> au -> IORef Bool -> IORef Bool -> System W
inputSys p au southHeld muted t w = do
  evs <- readEvents Event w
  fireNow <- foldlM onEvent False evs
  MkKeys held <- resource {c = Keys} w
  MkTouches fingers <- resource {c = Touches} w
  (sw, _) <- surfaceSize p
  pads <- gamepads p
  south <- readIORef southHeld
  let keySteer = heldAmount held rightKeys - heldAmount held leftKeys
      keyGas = heldAmount held gasKeys
      keyBrake = heldAmount held brakeKeys
      touchSteer = case fingers of
                     ((_, x, _) :: _) => clamp (-1.0) 1.0 ((x / max 1.0 sw - 0.5) * 3.0)
                     [] => 0.0
      touchGas = if null fingers then 0.0 else 1.0
      (padSteer, padGas, padBrake, padFire) = the (Double, Double, Double, Bool) $ case pads of
        (g :: _) => ( if abs g.leftX > 0.15 then g.leftX else 0.0
                    , max g.rightTrigger (if pressed East g then 1.0 else 0.0)
                    , g.leftTrigger
                    , pressed South g )
        [] => (0.0, 0.0, 0.0, False)
      fire = fireNow || (padFire && not south)
  writeIORef southHeld padFire
  cmap {q = (With Human, Pad)} {r = Pad} w $ \_ =>
    MkPad (clamp (-1.0) 1.0 (keySteer + touchSteer + padSteer))
          (clamp 0.0 1.0 (keyGas + touchGas + padGas))
          (clamp 0.0 1.0 (keyBrake + padBrake))
          fire
  where
    onEvent : Bool -> Event -> IO Bool
    onEvent f (KeyDown k) = do
      modifyResource w (\(MkKeys h) => MkKeys (k :: filter (/= k) h))
      case k of
        " " => pure True
        "c" => setResource w (MkFollow Nothing) >> pure f
        "m" => do
          m <- map not (readIORef muted)
          writeIORef muted m
          setMasterGain au (if m then 0.0 else 1.0)
          pure f
        _ => pure f
    onEvent f (KeyUp k) = modifyResource w (\(MkKeys h) => MkKeys (filter (/= k) h)) >> pure f
    onEvent f (TouchStart i x y) = do
      MkTouches fs <- resource w
      setResource w (MkTouches (fs ++ [(i, x, y)]))
      -- A second finger fires.
      pure (f || not (null fs))
    onEvent f (TouchMove i x y) =
      modifyResource w (\(MkTouches fs) => MkTouches (map (\(j, a, b) => if j == i then (j, x, y) else (j, a, b)) fs)) >> pure f
    onEvent f (TouchEnd i _ _) =
      modifyResource w (\(MkTouches fs) => MkTouches (filter (\(j, _, _) => j /= i) fs)) >> pure f
    onEvent f _ = pure f

||| Every car's position, for the systems that look at the others.
carsNow : World W -> IO (List (Entity, Kin, Racer))
carsNow w = cfold {q = (Entity, Kin, Racer)} w (\acc, x => x :: acc) []

||| The CPU drivers: aim at a point down the road in their lane, edging out
||| of the way of a car close ahead; slow for the turns they can see
||| coming; fire at whatever lines up in front.
export
aiSys : System W
aiSys t w = do
  cars <- carsNow w
  MkStandings _ _ <- resource {c = Standings} w
  humanProg <- cfold {q = (With Human, Racer)} w (\_, (_, r) => progress r) 0
  cmap {q = (Entity, Cpu, Kin, Racer, Weapon)} {r = Pad} w $ \(e, MkCpu skill lane, k, r, wp) =>
    let fwd = forwardOf k.heading
        vf = dot3 k.vel fwd
        look = 5.0 + abs vf * 0.32
        s = cast r.seg + look
        (_, tt) = along s
        blocked = any (\(o, ok, _) => o /= e
                          && let d = sub3 ok.pos k.pos in
                             dot3 d fwd > 0.0 && dot3 d fwd < 8.0
                             && abs (dot3 d (rightOf k.heading)) < 1.8) cars
        laneNow = if blocked then (if lane > 0.0 then lane - 2.6 else lane + 2.6) else lane
        (tp, _) = along s
        target = add3 tp (scale3 laneNow (sideOf tt))
        desired = atan2 (target.vz - k.pos.vz) (target.vx - k.pos.vx)
        diff = wrapAngle (desired - k.heading)
        bend = turnBetween (cast r.seg + 4.0) (cast r.seg + 4.0 + 16.0)
        -- A little help for those far behind you, a little restraint for
        -- those far ahead: the race stays a race.
        gap = cast (humanProg - progress r) / cast samples
        boost = clamp 0.92 1.06 (1.0 + 0.04 * gap)
        vmax = topSpeed * skill * boost * (1.0 - 0.5 * min 1.0 (bend / 1.1))
        aimed = any (\(o, ok, _) => o /= e
                        && let d = sub3 ok.pos k.pos
                               dist = length3 d in
                           dist > 2.0 && dist < 28.0
                           && abs (wrapAngle (atan2 d.vz d.vx - k.heading)) < 0.1) cars
        fire = aimed && wp.ammo > 0 && wp.cooldown <= 0.0
               && hashUnit (the Int (cast (t.time * 60.0)) + r.number * 97) < 0.08
     in MkPad (clamp (-1.0) 1.0 (diff * 2.4))
              (if vf < vmax then 1.0 else 0.0)
              (if vf > vmax + 3.0 then 1.0 else 0.0)
              fire

||| Driving: thrust and drag along the car, grip across it (less on grass,
||| so cars slide), steering that needs speed, and tyre walls that push
||| back. A spinning car ignores its controls until it stops, and one off
||| the ground has nothing to steer or push against: it flies until it
||| lands.
export
physicsSys : System W
physicsSys t w = do
  go <- racing w
  cmapM {q = (Entity, Kin, Pad, Spin, Racer, Maybe Human)} {r = (Kin, Spin)} w $
    \(e, k, pad0, MkSpin spin, r, human) => do
      let dt = t.delta
          pad = if go then pad0 else MkPad 0.0 0.0 0.0 False
          (_, lat0) = nearest r.seg k.pos
          onRoad = abs lat0 <= halfWidth + 0.8
          airborne = k.pos.vy > surface r.seg k.pos + 0.05
          top = case human of
                  Just _ => topSpeed
                  Nothing => topSpeed
          spinning = spin > 0.0
          h1 = if spinning then k.heading + 9.0 * dt else k.heading
          f0 = forwardOf h1
          vf0 = dot3 k.vel f0
          vr0 = dot3 k.vel (rightOf h1)
          steerRate = 2.7 * pad.steer * clamp 0.0 1.0 (abs vf0 / 7.0)
                      * (if vf0 < 0.0 then -1.0 else 1.0)
          h = if spinning || airborne then h1 else h1 + steerRate * dt
          f = forwardOf h
          vf1 = dot3 k.vel f
          vr1 = dot3 k.vel (rightOf h)
          accel = if spinning || airborne then 0.0 else
                    pad.throttle * 24.0 * max 0.0 (1.0 - vf1 / top)
                    - pad.brake * (if vf1 > 0.5 then 38.0 else 10.0 * (1.0 + vf1 / 8.0))
          drag = if airborne then 0.05 else if onRoad then 0.12 else 1.5
          vf2 = (vf1 + accel * dt) * exp (-(drag + (if spinning && not airborne then 1.5 else 0.0)) * dt)
          grip = if airborne then 0.0 else if spinning then 1.5 else if onRoad then 7.5 else 2.5
          vr2 = vr1 * exp (-grip * dt)
          vh = add3 (scale3 vf2 f) (scale3 vr2 (rightOf h))
          ph = add3 (flat k.pos) (scale3 dt vh)
          (seg, lat) = nearest r.seg ph
          (c, tt) = along (cast seg)
          n = sideOf tt
          -- Up and down: the road holds a car up, and pushing it up a ramp
          -- gives it the climb it leaves the lip with.
          g = surface seg ph
          y1 = k.pos.vy + k.vel.vy * dt
          (y, vy) = the (Double, Double) $
            if y1 <= g then (g, max 0.0 ((g - k.pos.vy) / max 0.001 dt)) else (y1, k.vel.vy - gravity * dt)
          landed = airborne && y1 <= g
          p = { vy := y } ph
          v = if landed then scale3 0.9 vh else { vy := vy } vh
      when (landed && k.vel.vy < -6.0) $ sendEvent w (Scrape p)
      if abs lat > wallReach
        then do
          let sign = if lat > 0.0 then 1.0 else -1.0
              p' = sub3 p (scale3 ((abs lat - wallReach) * sign) n)
              out = dot3 v n * sign
              v' = if out > 0.0 then sub3 v (scale3 (1.35 * out * sign) n) else v
          when (out > 3.0) $ sendEvent w (Scrape p')
          pure (MkKin p' h (scale3 0.93 v'), MkSpin (max 0.0 (spin - dt)))
        else pure (MkKin p h v, MkSpin (max 0.0 (spin - dt)))

||| Cars that touch push apart and trade the speed along the line between
||| them.
export
collideSys : System W
collideSys _ w = do
  cars <- carsNow w
  traverse_ (pairWith cars) cars
  where
    resolve : Entity -> (Entity, Kin, Racer) -> IO ()
    resolve a (b, _, _) =
      when (entityIndex a < entityIndex b) $ do
        Just ka <- getComponent {c = Kin} w a | Nothing => pure ()
        Just kb <- getComponent {c = Kin} w b | Nothing => pure ()
        let d = flat (sub3 kb.pos ka.pos)
            dist = length3 d
        when (dist < 2.0 && dist > 0.001 && abs (kb.pos.vy - ka.pos.vy) < 1.2) $ do
          let n = scale3 (1.0 / dist) d
              push = scale3 ((2.0 - dist) * 0.5) n
              rel = dot3 (flat (sub3 kb.vel ka.vel)) n
              j = if rel < 0.0 then scale3 (-rel * 0.9) n else zero3
          insert w a (MkKin (sub3 ka.pos push) ka.heading (sub3 ka.vel j))
          insert w b (MkKin (add3 kb.pos push) kb.heading (add3 kb.vel j))
          when (rel < -4.0) $ sendEvent w (Scrape (midpoint ka.pos kb.pos))

    pairWith : List (Entity, Kin, Racer) -> (Entity, Kin, Racer) -> IO ()
    pairWith cars (a, _, _) = traverse_ (resolve a) cars

||| Lap timing: crossing the start line after passing halfway completes a
||| lap and refills the missiles. The first crossing, from the grid,
||| starts the clock.
export
raceSys : System W
raceSys t w = do
  cmapM {q = (Kin, Racer, Weapon, Maybe Human)} {r = (Racer, Weapon)} w $ \(k, r, wp, human) => do
    let (seg, _) = nearest r.seg k.pos
        crossed = r.seg > samples - 40 && seg < 40
        halfway = abs (seg - samples `div` 2) < 30
        wp' = MkWeapon wp.ammo (max 0.0 (wp.cooldown - t.delta))
    if crossed
      then if r.armed
        then do
          let lap = t.time - r.lapStart
          when (isJust human) $ sendEvent w Lap
          pure ( { seg := seg, crossings $= (+ 1), armed := False, lapStart := t.time
                 , lastLap := lap
                 , bestLap := if r.bestLap <= 0.0 then lap else min r.bestLap lap } r
               , { ammo := maxAmmo } wp' )
        else pure ({ seg := seg, crossings $= (+ 1), lapStart := t.time } r, wp')
      else pure ({ seg := seg, armed := r.armed || halfway } r, wp')
  -- Standings: your place among everyone's progress.
  cars <- carsNow w
  mine <- cfold {q = (With Human, Racer)} w (\_, (_, r) => progress r) 0
  let ahead = length (filter (\(_, _, r) => progress r > mine) cars)
  setResource w (MkStandings (cast ahead + 1) (cast (length cars)))

||| Firing: a missile leaves the nose at the car's speed plus its own.
export
weaponSys : System W
weaponSys _ w = do
  go <- racing w
  cmapM {q = (Entity, Kin, Pad, Racer, Weapon)} {r = Weapon} w $ \(e, k, pad, r, wp) =>
    if go && pad.fire && wp.ammo > 0 && wp.cooldown <= 0.0
      then do
        let f = forwardOf k.heading
            p = add3 (add3 k.pos (scale3 1.6 f)) (v3 0.0 0.5 0.0)
        _ <- spawn w (MkMissile e r.seg p (add3 (flat k.vel) (scale3 55.0 f)) 1.3)
        sendEvent w (Fire p)
        pure ({ ammo $= \a => a - 1, cooldown := 0.45 } wp)
      else pure wp

||| Missiles fly straight, settling to skim the road; one that reaches
||| another car spins it out in a burst of sparks, one that reaches a wall
||| bursts there.
export
missileSys : System W
missileSys t w = do
  cars <- carsNow w
  cfor {q = (Entity, Missile)} w $ \(m, ms) => do
    let p0 = add3 ms.mpos (scale3 t.delta ms.mvel)
        (seg, lat) = nearest ms.mseg p0
        p = { vy := max (surface seg p0 + 0.5) (p0.vy - 6.0 * t.delta) } p0
        life = ms.mlife - t.delta
        hit = find (\(c, k, _) => c /= ms.owner && length3 (sub3 (add3 k.pos (v3 0.0 0.5 0.0)) p) < 1.5) cars
    case hit of
      Just (c, _, r) => do
        insert w c (MkSpin 1.4)
        burst w p 40 0.07 0.6 (r.number * 31 + the Int (cast (t.time * 100.0)))
        sendEvent w (Boom p)
        despawn w m
      Nothing =>
        if life <= 0.0 || abs lat > wallReach + 0.8
          then do
            burst w p 14 0.1 0.5 (the Int (cast (t.time * 100.0)))
            despawn w m
          else insert w m (MkMissile ms.owner seg p ms.mvel life)

||| Sparks fall, slow, age and go.
export
sparkSys : System W
sparkSys t w = do
  cmap {q = (Pos, Vel)} w $ \(MkPos p, MkVel v) =>
    let v' = scale3 (exp (-1.2 * t.delta)) ({ vy $= \y => y - 18.0 * t.delta } v)
        p' = add3 p (scale3 t.delta v')
     in (MkPos ({ vy $= max 0.05 } p'), MkVel v')
  cmapM {q = (Entity, Life)} w $ \(e, MkLife a s) =>
    if a + t.delta >= s then despawn w e >> pure (MkLife a s)
                        else pure (MkLife (a + t.delta) s)

||| Car roots take their pose from their motion, nose up or down with the
||| climb.
export
poseSys : System W
poseSys _ w = cmap {q = (Kin, Transform)} {r = Transform} w $ \(k, tf) =>
  let pitch = clamp (-0.6) 0.6 (atan2 k.vel.vy (max 1.0 (length3 (flat k.vel)))) in
  withRotation (axisAngle (v3 0.0 1.0 0.0) (-k.heading) `qmul` axisAngle (v3 0.0 0.0 1.0) pitch) (at k.pos)

paintSys : System W
paintSys _ w = do
  cmap {q = (Pos, Life, Hue)} {r = Instance Spark} w $ \(MkPos p, MkLife a s, MkHue h sat l) =>
    let k = a / s in
    MkInstance (translate p.vx p.vy p.vz `mmul` scaleM (0.16 * (1.0 - 0.7 * k)))
               (hsl h sat (l * (1.0 - 0.5 * k)))
  cmap {q = Missile} {r = Instance Rocket} w $ \ms =>
    let h = atan2 ms.mvel.vz ms.mvel.vx in
    MkInstance (translate ms.mpos.vx ms.mpos.vy ms.mpos.vz
                 `mmul` matOfQuat (axisAngle (v3 0.0 1.0 0.0) (-h))
                 `mmul` scaleXYZ 0.7 0.18 0.18)
               (rgb 1.0 0.85 0.4)
  -- Shadows: on the road below each car, cast away from the sun.
  cmap {q = (Kin, Racer)} {r = Instance Shade} w $ \(k, r) =>
    let g = surface r.seg k.pos
        up = k.pos.vy - g
     in MkInstance (translate (k.pos.vx - 0.5 * up) (g + 0.05) (k.pos.vz - 0.35 * up)
                     `mmul` matOfQuat (axisAngle (v3 0.0 1.0 0.0) (-k.heading))
                     `mmul` scaleXYZ 2.3 0.02 1.3)
                   (rgb 0.03 0.035 0.03)

||| The camera eases after the followed car, looking a little ahead of it.
cameraSys : System W
cameraSys t w = do
  MkFollow fol <- resource {c = Follow} w
  target <- the (IO (Maybe Kin)) $ case fol of
    Just e => getComponent {c = Kin} w e
    Nothing => pure (the (Maybe Kin) Nothing)
  k <- the (IO (Maybe Kin)) $ case target of
    Just k => pure (Just k)
    Nothing => cfold {q = (With Human, Kin)} w (\_, (_, k) => Just k) (the (Maybe Kin) Nothing)
  case k of
    Nothing => pure ()
    Just k => modifyResource w $ \(MkView f) =>
      let goal = flat (add3 k.pos (scale3 0.35 k.vel))
          a = 1.0 - exp (-4.0 * t.delta)
       in MkView (add3 f (scale3 a (sub3 goal f)))

hudSys : Status -> System W
hudSys status _ w = do
  MkStandings place fieldSize <- resource {c = Standings} w
  MkFollow fol <- resource {c = Follow} w
  MkCountdown left <- resource {c = Countdown} w
  mine <- cfold {q = (With Human, Racer, Weapon)} w (\_, (_, r, wp) => Just (r, wp)) (the (Maybe (Racer, Weapon)) Nothing)
  case mine of
    Nothing => pure ()
    Just (r, wp) => do
      status "count-label" $
        if left > 0.0 then "starting in " ++ show (the Int (cast (ceiling left)))
        else if left > -1.0 then "GO!"
        else "P" ++ show place ++ "/" ++ show fieldSize ++ "  lap " ++ show (max 1 (r.crossings + 1))
      status "stats" ("last " ++ lapTime r.lastLap ++ "  best " ++ lapTime r.bestLap
                      ++ "  missiles " ++ pack (replicate (cast wp.ammo) '*'))
      status "note" $ case fol of
        Nothing => "arrows/WASD drive, Space fires, click a car to follow it, M mutes"
        Just _ => "following a CPU car: click yours or press C to come back"

||| Engine voices, made on the first input -- a browser drops sound until a
||| user gesture, and a loop dropped at startup would never come back.
record Engine where
  constructor MkEngine
  low, high : Voice

||| Sound: cues from `Events Sfx`, panned and attenuated by where they
||| happened relative to the camera; and your engine, two loops crossfaded
||| by speed.
audioSys : Audio au => au -> IORef (Maybe Engine) -> (SoundHandle, SoundHandle, SoundHandle, SoundHandle, SoundHandle, SoundHandle)
        -> System W
audioSys au eng (lowS, highS, fireS, boomS, lapS, scrapeS) _ w = do
  inputs <- readEvents Event w
  started <- readIORef eng
  when (isNothing started && any gesture inputs) $ do
    lo <- play au lowS (withGain 0.0 (looping playback))
    hi <- play au highS (withGain 0.0 (looping playback))
    writeIORef eng (Just (MkEngine lo hi))
  MkView focus <- resource {c = View} w
  cues <- readEvents Sfx w
  traverse_ (cue focus) cues
  Just e <- readIORef eng | Nothing => pure ()
  speed <- cfold {q = (With Human, Kin)} w (\_, (_, k) => length3 (flat k.vel)) 0.0
  let s = clamp 0.0 1.0 (speed / topSpeed)
  setVoice au e.low (0.5 * (1.0 - s) + 0.1) 0.0
  setVoice au e.high (0.45 * s) 0.0
  where
    gesture : Event -> Bool
    gesture (KeyDown _) = True
    gesture (PointerDown _ _ _) = True
    gesture (TouchStart _ _ _) = True
    gesture _ = False

    placed : V3 -> V3 -> Playback -> Playback
    placed focus p pb =
      let d = sub3 p focus
       in withGain (pb.gain * clamp 0.15 1.0 (1.4 - length3 d / 30.0))
            (panned (clamp (-1.0) 1.0 (dot3 d screenRight / 16.0)) pb)

    cue : V3 -> Sfx -> IO ()
    cue focus (Fire p) = ignore (play au fireS (placed focus p (withGain 0.8 playback)))
    cue focus (Boom p) = ignore (play au boomS (placed focus p playback))
    cue focus (Scrape p) = ignore (play au scrapeS (placed focus p (withGain 0.7 playback)))
    cue _ Lap = ignore (play au lapS playback)

||| A click or tap on a car follows it; on yours, comes back.
pickSys : Platform p => p -> Picker Entity -> System W
pickSys p picker _ w = do
  evs <- readEvents Event w
  targets <- pickTargets w
  sfc <- surfaceSize p
  MkView focus <- resource {c = View} w
  picks <- pickEvents picker (camera focus) sfc targets evs
  traverse_ choose picks
  where
    choose : PickEvent Entity -> IO ()
    choose (PickClick _ _ e _) = do
      mine <- holds Human w e
      setResource w (MkFollow (if mine then Nothing else Just e))
    choose _ = pure ()

||| The tyre walls and trees never change after the first frame: change
||| detection uploads them once and never again.
staticSys : Renderer r f => r -> InstanceHandle -> InstanceHandle -> System W
staticSys r tyreI pineI _ w = do
  t <- cfold {q = Changed (Instance Tyre)} w (\n, _ => n + 1) (the Int 0)
  when (t > 0) $ uploadBatch Tyre r tyreI w
  p <- cfold {q = Changed (Instance Pine)} w (\n, _ => n + 1) (the Int 0)
  when (p > 0) $ uploadBatch Pine r pineI w

--------------------------------------------------------------------------------
-- Building the world

record Meshes where
  constructor MkMeshes
  cube, wheel, cone, ball : MeshHandle Triangles

||| The grid slot you start from: mid-pack, as Supercars 2 starts you.
humanSlot : Int
humanSlot = 5

||| The race without its senses: every system that decides what happens,
||| none that reads input or makes light or sound. What `Checks/RacerCheck`
||| runs headless.
||| An empty race world, with room for every car, missile and spark.
export
newRaceWorld : IO (World W)
newRaceWorld = newWorld {cs = W} maxEntities

export
simulation : List (System W)
simulation = [startSys, aiSys, physicsSys, collideSys, raceSys, weaponSys, missileSys, sparkSys, poseSys]

||| Counts missile launches and hits, for the check.
export
tallyFights : IORef Int -> IORef Int -> System W
tallyFights fired hits _ w = do
  evs <- readEvents Sfx w
  traverse_ count evs
  where
    count : Sfx -> IO ()
    count (Fire _) = modifyIORef fired (+ 1)
    count (Boom _) = modifyIORef hits (+ 1)
    count _ = pure ()

||| The highest any car is off the ground now, for the check.
export
highest : World W -> IO Double
highest w = cfold {q = (Kin, Racer)} w (\m, (k, r) => max m (k.pos.vy - surface r.seg k.pos)) 0.0

||| A summary of every car for the check: slot, completed laps, best lap.
export
results : World W -> IO (List (Int, Int, Double))
results w = do
  rs <- cfold {q = Racer} w (\acc, r => (r.number, r.crossings, r.bestLap) :: acc) []
  pure (sortBy (\(a, _, _), (b, _, _) => compare a b) rs)

||| Your car is red; the CPU cars take the rest, in grid order.
carColours : List Color
carColours =
  [ srgb 0.15 0.35 0.85, srgb 0.95 0.8 0.15, srgb 0.15 0.6 0.3, srgb 0.9 0.9 0.9
  , srgb 0.55 0.2 0.75, srgb 0.85 0.1 0.08, srgb 0.95 0.5 0.1, srgb 0.15 0.7 0.75 ]

||| A car's racing self, on its grid slot: everything but its looks. A
||| `human` car takes its controls from input; the rest from the AI.
export
spawnCar : World W -> (slot : Int) -> (human : Bool) -> IO (Maybe Entity)
spawnCar w k human = do
  let row = k `div` 2
      col = if k `mod` 2 == 0 then 1.8 else -1.8
      s = cast (samples - 4 - row * 4)
      (c, t) = along s
      p = add3 c (scale3 col (sideOf t))
      h = headingOf t
  Just root <- spawn w $ the (Transform, Kin, Pad, Racer, Weapon, Spin, Pickable)
                       ( (withRotation (axisAngle (v3 0.0 1.0 0.0) (-h)) (at p))
                       , MkKin p h zero3, MkPad 0.0 0.0 0.0 False
                       , MkRacer k (cast s) (-1) False 0.0 0.0 0.0
                       , MkWeapon maxAmmo 0.0, MkSpin 0.0
                       , MkPickable (BoundSphere (v3 0.0 0.5 0.0) 1.6) )
    | Nothing => pure Nothing
  if human
    then insert w root MkHuman
    else insert w root (MkCpu (0.9 + 0.1 * hashUnit (k * 11)) (2.4 * hashUnit (k * 5) - 1.2))
  pure (Just root)

||| One car: a root that moves, and six parts that ride it.
buildCar : Renderer r f => r -> MaterialId StandardMaterial -> Meshes
        -> Handle StandardMaterial -> Handle StandardMaterial
        -> World W -> (Int, Color) -> IO ()
buildCar r mid ms glassH tyreH w (k, colour) = do
  bodyH <- addMaterial r mid (withMetallic 0.3 (withRoughness 0.35 (lit colour)))
  Just root <- spawnCar w k (k == humanSlot) | Nothing => pure ()
  let part = \d : Drawable, tf : Transform => ignore (spawn w (tf, MkRenderable d, MkChildOf root))
      box = \x, y, z, sx, sy, sz => withScale (v3 sx sy sz) (at (v3 x y z))
      wheelAt = \x, z => withScale (v3 0.62 0.24 0.62)
                           (withRotation (axisAngle (v3 1.0 0.0 0.0) (pi / 2.0)) (at (v3 x 0.31 z)))
  part (MkDrawable ms.cube bodyH) (box 0.0 0.45 0.0 2.2 0.42 1.15)
  part (MkDrawable ms.cube glassH) (box (-0.15) 0.82 0.0 1.1 0.36 0.95)
  traverse_ (\(x, z) => part (MkDrawable ms.wheel tyreH) (wheelAt x z))
            (the (List (Double, Double)) [(0.72, 0.6), (0.72, -0.6), (-0.72, 0.6), (-0.72, -0.6)])


||| The circuit and everything around it.
buildTrack : Renderer r f => r -> MaterialId StandardMaterial -> MaterialId Hologram
          -> Meshes -> World W -> IO ()
buildTrack r mid holoId ms w = do
  gm <- registerGizmos r
  grassH <- addMaterial r mid (withRoughness 0.6 (withPattern (Noise 0.08) (lit (srgb 0.2 0.42 0.15))))
  tarmacH <- addMaterial r mid (withRoughness 0.6 (withPattern (Noise 0.6) (lit (srgb 0.27 0.27 0.3))))
  redH <- addMaterial r mid (withRoughness 0.6 (lit (srgb 0.8 0.12 0.1)))
  whiteH <- addMaterial r mid (withRoughness 0.6 (lit (srgb 0.88 0.88 0.88)))
  lineH <- addMaterial r mid (withPattern (Checker 0.5) (unlit white))
  marble <- loadTexture r (FromBase64 marbleJpgMime marbleJpg)
  standH <- addMaterial r mid (withTexture marble (withRoughness 0.7 (lit (srgb 0.8 0.8 0.85))))
  postH <- addMaterial r mid (withMetallic 0.7 (lit (srgb 0.6 0.62 0.66)))
  banner <- addMaterial r holoId (MkHologram (withAlpha 0.7 (hsl 0.55 0.9 0.6)) 6.0)
  -- Roughness stays near 0.6 throughout: rougher, and the shader's broad
  -- Blinn-Phong lobe -- under a fixed sun and a fixed isometric eye --
  -- lays the same white sheen over every upward face.
  ground <- loadMesh r (plane 400.0 400.0)
  road <- loadMesh r (fromList roadTris)
  kerbR <- loadMesh r (fromList (kerbTris True))
  kerbW <- loadMesh r (fromList (kerbTris False))
  ramp <- loadMesh r (fromList rampTris)
  concreteH <- addMaterial r mid (withRoughness 0.6 (withPattern (Noise 0.3) (lit (srgb 0.55 0.53 0.5))))
  let still = \d, tf => ignore (spawn w (tf, MkRenderable d))
  still (MkDrawable ground grassH) (at (v3 0.0 (-0.02) 0.0))
  still (MkDrawable ramp concreteH) neutral
  still (MkDrawable road tarmacH) neutral
  still (MkDrawable kerbR redH) neutral
  still (MkDrawable kerbW whiteH) neutral
  -- The dashed centre line: a retained gizmo, drawn like any mesh.
  dashes <- gizmoAsset r gm white (some (fromList centreDashes))
  still dashes neutral
  -- The start line, the gantry over it, and its holographic banner.
  let (c0, t0) = along 0.0
      h0 = headingOf t0
      across = sideOf t0
      onLine = \p, tf => withRotation (axisAngle (v3 0.0 1.0 0.0) (-h0)) (at p) `withScaleOf` tf
  still (MkDrawable ms.cube lineH) (onLine (add3 c0 (v3 0.0 0.03 0.0)) (v3 1.2 0.02 (halfWidth * 2.0)))
  traverse_ (\sgn => still (MkDrawable ms.cube postH)
                       (onLine (add3 c0 (add3 (scale3 (sgn * (wallReach + 0.5)) across) (v3 0.0 2.5 0.0)))
                               (v3 0.4 5.0 0.4)))
            (the (List Double) [1.0, -1.0])
  still (MkDrawable ms.cube banner) (onLine (add3 c0 (v3 0.0 5.2 0.0)) (v3 0.5 1.1 (wallReach * 2.0 + 1.0)))
  -- A grandstand beside the start straight, marble-clad.
  traverse_ (\i => still (MkDrawable ms.cube standH)
                     (onLine (add3 (fst (along (cast (i * 6 + 6))))
                                   (add3 (scale3 (-(wallReach + 5.0)) (sideOf (snd (along (cast (i * 6 + 6))))))
                                         (v3 0.0 1.2 0.0)))
                             (v3 5.0 2.4 4.0)))
            (the (List Int) [0, 1, 2, 3])
  -- Tyre walls and trees: instanced, uploaded once.
  traverse_ (\(p, red) => spawn w (MkInstance {b = Tyre} (translate p.vx 0.35 p.vz `mmul` scaleXYZ 0.9 0.7 0.9)
                                     (if red then srgb 0.75 0.12 0.1 else srgb 0.85 0.85 0.85)))
            tyres
  traverse_ (\(p, sz) => spawn w (MkInstance {b = Pine} (translate p.vx (1.4 * sz) p.vz `mmul` scaleXYZ (1.3 * sz) (2.8 * sz) (1.3 * sz))
                                     (hsl (0.3 + 0.05 * sz) 0.6 (0.12 + 0.04 * sz))))
            trees
  where
    withScaleOf : Transform -> V3 -> Transform
    withScaleOf tf s = withScale s tf

--------------------------------------------------------------------------------
-- The frame

lights : Lights
lights = Offler.Light.lights 0.18
  [ MkDirectional (v3 (-0.5) (-1.0) (-0.35)) (rgb 0.8 0.77 0.7)
  , MkDirectional (v3 0.6 (-0.4) 0.4) (rgb 0.12 0.14 0.2) ]

||| Lines over the world: missile trails, and rings under your car and
||| under the one the pointer is over.
overlay : World W -> Maybe Entity -> IO Gizmos
overlay w hover = do
  trails <- cfold {q = Missile} w (\acc, ms =>
              gLine (rgb 1.0 0.7 0.3) ms.mpos (sub3 ms.mpos (scale3 0.05 ms.mvel)) :: acc) []
  mine <- cfold {q = (With Human, Kin)} w (\_, (_, k) => Just k) (the (Maybe Kin) Nothing)
  over <- maybe (pure Nothing) (getComponent {c = Kin} w) hover
  let ring = \c, k => some (gCircle c (add3 k.pos (v3 0.0 0.08 0.0)) (v3 0.0 1.0 0.0) 1.7 24)
  pure (gconcat (map some trails
                 ++ maybe [] (\k => [ring (rgb 1.0 0.9 0.2) k]) mine
                 ++ maybe [] (\k => [ring (rgb 0.4 0.9 1.0) k]) over))

record Batches where
  constructor MkBatches
  sparkI, rocketI, tyreI, pineI, shadeI : InstanceHandle

frame : Renderer r f => r -> World W -> Schedule W -> Clock -> Picker Entity
     -> Meshes -> Handle StandardMaterial -> Handle StandardMaterial
     -> Handle StandardMaterial -> Batches
     -> List Event -> Double -> L IO ()
frame r w sch clock picker ms sparkH plainH pineH bs evs t = do
  focus <- liftIO $ do
    sendEvents w evs
    tk <- tick clock t
    runSchedule sch tk w
    uploadBatch Spark r bs.sparkI w
    uploadBatch Rocket r bs.rocketI w
    uploadBatch Shade r bs.shadeI w
    hover <- hovered picker MousePointer
    overlay w hover >>= drawGizmoData r
    MkView f <- resource {c = View} w
    pure f
  Just fr <- beginFrame r (camera focus) lights t
    | Nothing => pure ()
  fr1 <- renderWorld r fr w
  fr2 <- drawInstanced r fr1 ms.cube plainH bs.tyreI identity
  fr3 <- drawInstanced r fr2 ms.cone pineH bs.pineI identity
  fr4 <- drawInstanced r fr3 ms.ball sparkH bs.sparkI identity
  fr5 <- drawInstanced r fr4 ms.cube sparkH bs.rocketI identity
  fr5' <- drawInstanced r fr5 ms.cube sparkH bs.shadeI identity
  fr6 <- drawGizmos r fr5'
  endFrame r fr6

export
race : Renderer r f => Platform p => Audio au => r -> p -> au -> Status -> IO ()
race r p au status = do
  w <- newRaceWorld
  mid <- registerMaterial {m = StandardMaterial} r
  holoId <- registerMaterial {m = Hologram} r
  ms <- [| MkMeshes (loadMesh r cube) (loadMesh r (cylinder 1.0 1.0 16))
                    (loadMesh r (cone 1.0 1.0 12)) (loadMesh r (sphere 1.0 1)) |]
  buildTrack r mid holoId ms w
  glassH <- addMaterial r mid (withMetallic 0.6 (withRoughness 0.2 (lit (srgb 0.12 0.14 0.18))))
  tyreH <- addMaterial r mid (withRoughness 0.9 (lit (srgb 0.08 0.08 0.08)))
  traverse_ (buildCar r mid ms glassH tyreH w) (zip [0 .. 7] carColours)
  -- Instance colours carry the look; one asset per batch.
  sparkH <- addMaterial r mid (unlit white)
  plainH <- addMaterial r mid (withRoughness 0.6 (lit white))
  pineH <- addMaterial r mid (withRoughness 0.6 (lit white))
  bs <- [| MkBatches (createInstances r) (createInstances r) (createInstances r) (createInstances r)
                     (createInstances r) |]
  sounds <- [| MkPair (loadSound au (SoundPcm Sound.rate (engine 70.0)))
             [| MkPair (loadSound au (SoundPcm Sound.rate (engine 140.0)))
             [| MkPair (loadSound au (SoundPcm Sound.rate launch))
             [| MkPair (loadSound au (SoundPcm Sound.rate boom))
             [| MkPair (loadSound au (SoundPcm Sound.rate chime))
                       (loadSound au (SoundPcm Sound.rate scrape)) |] |] |] |] |]
  eng <- newIORef Nothing
  southHeld <- newIORef False
  muted <- newIORef False
  picker <- newPicker
  sch <- newSchedule
    [ startSys, inputSys p au southHeld muted, pickSys p picker, aiSys, physicsSys
    , collideSys, raceSys, weaponSys, missileSys, sparkSys, poseSys
    , \_ => propagate, paintSys, cameraSys, hudSys status
    , audioSys au eng sounds, staticSys r bs.tyreI bs.pineI ]
  clock <- newClock 0.05
  fps <- newFps
  status "backend" (rendererName r)
  runLoop p $ \t => do
    evs <- pollEvents p
    LIO.run (frame r w sch clock picker ms sparkH plainH pineH bs evs t)
    reportFps status fps t

export
app : App
app = MkApp "Racer" race
