||| 2D rendering: an orthographic camera looking down -z at a court of
||| bouncing circles and rectangles, every one unlit -- flat colour, no
||| lighting, no perspective. Demonstrates the orthographic projection and
||| the unlit material, which together are the 2D mode of the same 3D
||| pipeline.
module Examples.Flat2d.Scene

import Data.List
import Examples.Util
import Offler.Camera
import Offler.Color
import Offler.Gfx.Platform
import Offler.Gfx.Renderer
import Offler.Light
import Offler.Material
import Offler.Math
import Offler.Mesh
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

||| Half-height of the court in world units; the width follows the window's
||| aspect ratio, so the walls sit at a fixed height and the sides breathe.
courtHalfH : Double
courtHalfH = 5.0

camera : Camera
camera = withClearColor (rgb 0.06 0.06 0.09)
           (orthographicCamera courtHalfH (at (v3 0.0 0.0 10.0)))

||| Unused by unlit materials, but `beginFrame` wants one.
lights : Lights
lights = defaultLights

hashUnit : Int -> Double
hashUnit i =
  let x = sin (cast i * 12.9898 + 78.233) * 43758.5453
   in x - floor x

||| Fold a line into [lo, hi] as a triangle wave: the position of a body
||| that bounces off the walls, computed statelessly from the time.
bounce : (lo, hi, x : Double) -> Double
bounce lo hi x =
  let w = hi - lo
      period = 2.0 * w
      y = x - lo
      m = y - period * floor (y / period)
   in if m < w then lo + m else hi - (m - w)

record Sprite where
  constructor MkSprite
  shape : Int          -- 0 circle, 1 square, 2 tall rectangle
  size, x0, y0, vx, vy, spin : Double
  mat : Material

mkSprite : Int -> Sprite
mkSprite i =
  MkSprite
    (cast (the Double (hashUnit (i * 7 + 1) * 3.0)))
    (0.35 + 0.55 * hashUnit (i * 3 + 8))
    (hashUnit (i * 5 + 2) * 12.0 - 6.0)
    (hashUnit (i * 9 + 4) * 8.0 - 4.0)
    (0.7 + 2.2 * hashUnit (i * 11 + 6))
    (0.7 + 2.2 * hashUnit (i * 13 + 7))
    ((hashUnit (i * 17 + 9) - 0.5) * 3.0)
    (unlit (hsl (hashUnit (i * 3 + 5)) 0.8 0.6))

sprites : List Sprite
sprites = map mkSprite (range 0 39)

||| Where a sprite is now: its start point carried along its velocity,
||| folded back into the court. The court is a fixed 12 wide for motion, so
||| the pattern is identical on every backend and aspect.
spriteModel : Double -> Sprite -> Mat4
spriteModel t s =
  let hw = 6.0
      hh = courtHalfH - 0.6
      x = bounce (-hw) hw (s.x0 + s.vx * t)
      y = bounce (-hh) hh (s.y0 + s.vy * t)
   in translate x y 0.0 `mmul` rotateZ (s.spin * t) `mmul` scaleM s.size

handle : Renderer r f => r -> Event -> IO ()
handle r Resized = resize r
handle _ _ = pure ()

drawSprites : Renderer r f => r -> (1 frame : f) -> Double
           -> (circle : MeshHandle) -> (square : MeshHandle) -> (tall : MeshHandle)
           -> List Sprite -> L1 IO f
drawSprites r fr _ _ _ _ [] = pure1 fr
drawSprites r fr t circle square tall (s :: rest) = do
  let m = if s.shape == 0 then circle else if s.shape == 1 then square else tall
  fr' <- draw r fr m (spriteModel t s) s.mat
  drawSprites r fr' t circle square tall rest

frame : Renderer r f => Platform p =>
        r -> p -> MeshHandle -> MeshHandle -> MeshHandle -> FpsCounter
      -> Double -> L IO ()
frame r p circleM squareM tallM fps t = do
  liftIO (pollEvents p >>= traverse_ (handle r))
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- drawSprites r fr t circleM squareM tallM sprites
  endFrame r fr1

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  circleM <- loadMesh r (circle 0.5 48)
  squareM <- loadMesh r (rectangle 1.0 1.0)
  tallM <- loadMesh r (rectangle 0.55 1.6)
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "stats" (show (length sprites) ++ " sprites, orthographic")
  runLoop p (\t => LIO.run (frame r p circleM squareM tallM fps t) >> reportFps p fps t)
