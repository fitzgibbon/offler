||| 2D rendering: an orthographic camera looking down -z at a court of
||| bouncing circles and rectangles, every material unlit -- the 2D mode of
||| the same 3D pipeline. Retained throughout: each sprite is a scene-graph
||| node with its own material asset, and a frame is forty `setTransform`s
||| and one `renderScene`.
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
import Offler.Scene
import Offler.Transform

%hide Control.Linear.LIO.fromInteger

%default covering

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
  colour : Color

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
    (hsl (hashUnit (i * 3 + 5)) 0.8 0.6)

sprites : List Sprite
sprites = map mkSprite (range 0 39)

spriteTransform : Double -> Sprite -> Transform
spriteTransform t s =
  let hw = 6.0
      hh = courtHalfH - 0.6
      x = bounce (-hw) hw (s.x0 + s.vx * t)
      y = bounce (-hh) hh (s.y0 + s.vy * t)
   in withRotation (axisAngle (v3 0.0 0.0 1.0) (s.spin * t))
        (uniformScale s.size (at (v3 x y 0.0)))

frame : Renderer r f => Platform p =>
        r -> p -> Scene -> List (NodeId, Sprite) -> FpsCounter
      -> Status -> Double -> L IO ()
frame r p sc nodes fps status t = do
  liftIO $ do
    _ <- pollEvents p
    traverse_ (\(n, s) => setTransform sc n (spriteTransform t s)) nodes
  Just fr <- beginFrame r camera lights t
    | Nothing => pure ()
  fr1 <- renderScene r fr sc
  endFrame r fr1

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  mid <- registerMaterial {m = StandardMaterial} r
  circleM <- loadMesh r (circle 0.5 48)
  squareM <- loadMesh r (rectangle 1.0 1.0)
  tallM <- loadMesh r (rectangle 0.55 1.6)
  sc <- newScene
  nodes <- traverse (\s => do
      h <- addMaterial r mid (unlit s.colour)
      let mesh = if s.shape == 0 then circleM
                 else if s.shape == 1 then squareM else tallM
      n <- spawn sc Nothing (spriteTransform 0.0 s) (Just (MkDrawable mesh h))
      pure (n, s))
    sprites
  fps <- newFps
  status "backend" (rendererName r)
  status "stats" (show (length sprites) ++ " sprite nodes, orthographic")
  runLoop p $ \t =>
    LIO.run (frame r p sc nodes fps status t) >> reportFps status fps t
