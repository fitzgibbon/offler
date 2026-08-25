||| A user-defined material, end to end: `Hologram` implements the
||| `Material` interface with its own WGSL and GLSL bodies, its own uniform
||| block, and `Blend` alpha -- everything the standard material does, done
||| in application code, which is the bevy `Material` trait's promise. The
||| scene also shows `Mask`: cutout panels whose texture's alpha is
||| discarded below the cutoff by the generated `offlerAlpha` helper.
|||
||| Three overlapping holograms orbit through each other over the panels:
||| blended draws are queued and sorted back to front at `endFrame`, so the
||| compositing is right whatever order they are drawn in. Retained
||| throughout: the panels, ground and holograms are scene-graph nodes over
||| material assets made at startup.
module Examples.Custom.Scene

import Data.List
import Data.Maybe
import Examples.Assets
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

--------------------------------------------------------------------------------
-- The custom material

||| A scanline hologram: tinted, fresnel-edged, always blended.
public export
record Hologram where
  constructor MkHologram
  tint : Color
  ||| Scanline density, in stripes per world unit.
  stripes : Double

||| The implementation is the whole of what a material is: one uniform lane
||| for the tint, one for the parameters, no textures, authored shader
||| bodies against the generated declarations (`g`, `o`, `m`, `VertexIn` /
||| the flat GLSL block members), and `Blend` alpha. `public export`, so the
||| registration bounds can reduce at the call site.
public export
Material Hologram where
  matFields = [MkField "tint" Vec4, MkField "params" Vec4]
  matTexCount = 0
  matTextureSlots = []
  alphaMode _ = Blend
  matTextures _ = []
  writeMat w h = do
    putColor w 0 h.tint
    putVec4 w 1 h.stripes 0.0 0.0 0.0

  matWgsl = """
    struct VsOut {
      @builtin(position) pos : vec4<f32>,
      @location(0) nrm   : vec3<f32>,
      @location(1) world : vec3<f32>,
    };

    @vertex
    fn vs(v : VertexIn) -> VsOut {
      var out : VsOut;
      let world = o.model * vec4<f32>(v.pos, 1.0);
      out.world = world.xyz;
      out.nrm = (o.model * vec4<f32>(v.normal, 0.0)).xyz;
      out.pos = g.proj * g.view * world;
      return out;
    }

    @fragment
    fn fs(in : VsOut) -> @location(0) vec4<f32> {
      let N = normalize(in.nrm);
      let V = normalize(g.cam - in.world);
      let fresnel = pow(1.0 - abs(dot(N, V)), 2.0);
      let scan = 0.5 + 0.5 * sin(in.world.y * m.params.x - g.time * 2.5);
      let a = m.tint.a * (0.12 + 0.88 * fresnel) * (0.35 + 0.65 * scan);
      let colour = m.tint.rgb * (0.5 + 0.8 * fresnel) + vec3<f32>(0.25) * fresnel;
      return vec4<f32>(pow(colour, vec3<f32>(0.4545)), a);
    }
    """

  matGlslVert = """
    #version 300 es
    out vec3 vNormal;
    out vec3 vWorld;
    void main() {
      vec4 world = model * vec4(pos, 1.0);
      vWorld = world.xyz;
      vNormal = mat3(model) * normal;
      gl_Position = proj * view * world;
    }
    """

  matGlslFrag = """
    #version 300 es
    precision highp float;
    in vec3 vNormal;
    in vec3 vWorld;
    out vec4 outColour;
    void main() {
      vec3 N = normalize(vNormal);
      vec3 V = normalize(cam - vWorld);
      float fresnel = pow(1.0 - abs(dot(N, V)), 2.0);
      float scan = 0.5 + 0.5 * sin(vWorld.y * params.x - time * 2.5);
      float a = tint.a * (0.12 + 0.88 * fresnel) * (0.35 + 0.65 * scan);
      vec3 colour = tint.rgb * (0.5 + 0.8 * fresnel) + vec3(0.25) * fresnel;
      outColour = vec4(pow(colour, vec3(0.4545)), a);
    }
    """

--------------------------------------------------------------------------------
-- The scene

camera : Double -> Camera
camera t =
  let yaw = t * 0.08
      eye = v3 (10.0 * sin yaw) 4.0 (10.0 * cos yaw)
   in perspectiveCamera (lookingAt (v3 0.0 1.6 0.0) (v3 0.0 1.0 0.0) (at eye))

lights : Lights
lights = defaultLights

groundMat : TextureHandle -> StandardMaterial
groundMat t = withTexture t (withRoughness 0.95 (lit (srgb 0.75 0.78 0.88)))

||| Cutout panels: the texture's checkerboard alpha, masked at 0.5, so half
||| the squares are simply not there -- including their depth, which is
||| what distinguishes `Mask` from `Blend`.
panelMat : TextureHandle -> StandardMaterial
panelMat t =
  withAlpha (Mask 0.5) (withTexture t (withRoughness 0.8 (lit (srgb 0.9 0.95 0.9))))

holo : Double -> Double -> Hologram
holo hue stripes = MkHologram (withAlpha 0.75 (hsl hue 0.85 0.6)) stripes

holoTransform : Double -> Double -> Transform
holoTransform t phase =
  let a = t * 0.4 + phase
   in withRotation (fromEulerYXZ (t * 0.5) 0.0 0.0)
        (at (v3 (2.2 * cos a) 1.7 (2.2 * sin a)))

panelTransform : Int -> Transform
panelTransform i =
  let x = (cast i - 1.0) * 3.2
   in withRotation (axisAngle (v3 0.0 1.0 0.0) (cast i * 0.5 - 0.5))
        (at (v3 x 1.4 (-1.5)))

frame : Renderer r f => Platform p =>
        r -> p -> Scene -> List NodeId -> FpsCounter
      -> Status -> Double -> L IO ()
frame r p sc holos fps status t = do
  liftIO $ do
    _ <- pollEvents p
    traverse_ (\(k, n) => setTransform sc n (holoTransform t (cast k * 2.1)))
              (zip (range 0 (cast (length holos) - 1)) holos)
  Just fr <- beginFrame r (camera t) lights t
    | Nothing => pure ()
  fr1 <- renderScene r fr sc
  endFrame r fr1

export
run : Renderer r f => Platform p => r -> p -> Status -> IO ()
run r p status = do
  stdId <- registerMaterial {m = StandardMaterial} r
  holoId <- registerMaterial {m = Hologram} r
  marble <- loadTexture r (FromBase64 marbleJpgMime marbleJpg)
  cutout <- loadTexture r (FromBase64 cutoutPngMime cutoutPng)
  ground <- loadMesh r (plane 22.0 22.0)
  panel <- loadMesh r (cuboid 2.2 2.6 0.12)
  torusM <- loadIndexed r (torusIndexed 0.7 0.28 48 24)
  sphereM <- loadMesh r (sphere 0.9 3)
  coneM <- loadMesh r (cone 0.8 1.6 48)
  groundH <- addMaterial r stdId (groundMat marble)
  panelH <- addMaterial r stdId (panelMat cutout)
  holoHs <- traverse (addMaterial r holoId)
              [holo 0.55 9.0, holo 0.08 12.0, holo 0.85 7.0]
  sc <- newScene
  _ <- spawn sc Nothing neutral (Just (MkDrawable ground groundH))
  _ <- traverse (\i => spawn sc Nothing (panelTransform i)
                          (Just (MkDrawable panel panelH)))
        (range 0 2)
  holos <- traverse (\(mesh, h) =>
               spawn sc Nothing (holoTransform 0.0 0.0) (Just (MkDrawable mesh h)))
             (zip [torusM, sphereM, coneM] holoHs)
  fps <- newFps
  status "backend" (rendererName r)
  status "stats" "custom material assets (Blend) + mask cutouts"
  runLoop p $ \t =>
    LIO.run (frame r p sc holos fps status t) >> reportFps status fps t

export
app : App
app = MkApp "Custom" (soundless run)
