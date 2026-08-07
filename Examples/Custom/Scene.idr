||| A user-defined material, end to end: `Hologram` implements the
||| `Material` interface with its own WGSL and GLSL bodies, its own uniform
||| block, and `Blend` alpha -- everything the standard material does, done
||| in application code, which is the bevy `Material` trait's promise. The
||| scene also shows `Mask`: cutout panels whose texture's alpha is
||| discarded below the cutoff by the generated `offlerAlpha` helper.
|||
||| Three overlapping holograms orbit through each other over the panels:
||| blended draws are queued and sorted back to front at `endFrame`, so the
||| compositing is right whatever order they are drawn in.
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

groundMat : Maybe TextureHandle -> StandardMaterial
groundMat t =
  case t of
    Just h => withTexture h (withRoughness 0.95 (lit (srgb 0.75 0.78 0.88)))
    Nothing => withRoughness 0.95 (lit (srgb 0.42 0.44 0.50))

||| Cutout panels: the texture's checkerboard alpha, masked at 0.5, so half
||| the squares are simply not there -- including their depth, which is what
||| distinguishes `Mask` from `Blend`.
panelMat : Maybe TextureHandle -> StandardMaterial
panelMat t =
  let base = withRoughness 0.8 (lit (srgb 0.9 0.95 0.9))
   in withAlpha (Mask 0.5) (case t of
        Just h => withTexture h base
        Nothing => base)

holo : Double -> Double -> Hologram
holo hue stripes = MkHologram (withAlpha 0.75 (hsl hue 0.85 0.6)) stripes

holoModel : Double -> Double -> Double -> Mat4
holoModel t phase radius =
  let a = t * 0.4 + phase
   in matOf (withRotation (fromEulerYXZ (t * 0.5) 0.0 0.0)
              (at (v3 (radius * cos a) 1.7 (radius * sin a))))

panelModel : Int -> Mat4
panelModel i =
  let x = (cast i - 1.0) * 3.2
   in matOf (withRotation (axisAngle (v3 0.0 1.0 0.0) (cast i * 0.5 - 0.5))
              (at (v3 x 1.4 (-1.5))))

handle : Renderer r f => r -> Event -> IO ()
handle r Resized = resize r
handle _ _ = pure ()

drawPanels : Renderer r f => r -> (1 frame : f)
          -> MaterialId StandardMaterial -> StandardMaterial -> MeshHandle
          -> List Int -> L1 IO f
drawPanels r fr _ _ _ [] = pure1 fr
drawPanels r fr mid mat mesh (i :: rest) = do
  fr' <- draw r fr mid mesh (panelModel i) mat
  drawPanels r fr' mid mat mesh rest

frame : Renderer r f => Platform p =>
        r -> p
      -> MaterialId StandardMaterial -> MaterialId Hologram
      -> (ground, panel, torusM, sphereM, coneM : MeshHandle)
      -> StandardMaterial -> StandardMaterial -> FpsCounter
      -> Double -> L IO ()
frame r p stdId holoId ground panel torusM sphereM coneM gm pm fps t = do
  liftIO (pollEvents p >>= traverse_ (handle r))
  Just fr <- beginFrame r (camera t) lights t
    | Nothing => pure ()
  fr1 <- draw r fr stdId ground identity gm
  fr2 <- drawPanels r fr1 stdId pm panel [0, 1, 2]
  -- The holograms are Blend: drawn here, before nothing in particular,
  -- composited last and back to front by the sorted transparent phase.
  fr3 <- draw r fr2 holoId torusM (holoModel t 0.0 2.2) (holo 0.55 9.0)
  fr4 <- draw r fr3 holoId sphereM (holoModel t 2.1 2.2) (holo 0.08 12.0)
  fr5 <- draw r fr4 holoId coneM (holoModel t 4.2 2.2) (holo 0.85 7.0)
  endFrame r fr5

export
run : Renderer r f => Platform p => r -> p -> IO ()
run r p = do
  stdId <- registerMaterial {m = StandardMaterial} r
  holoId <- registerMaterial {m = Hologram} r
  ground <- loadMesh r (plane 22.0 22.0)
  panel <- loadMesh r (cuboid 2.2 2.6 0.12)
  torusM <- loadMesh r (torus 0.7 0.28 48 24)
  sphereM <- loadMesh r (sphere 0.9 3)
  coneM <- loadMesh r (cone 0.8 1.6 48)
  fps <- newFps
  setStatus p "backend" (rendererName r)
  setStatus p "stats" "custom material (Blend) + mask cutouts"
  loadTexture r (FromBase64 marbleJpgMime marbleJpg) $ \marble =>
    loadTexture r (FromBase64 cutoutPngMime cutoutPng) $ \cutout => do
      when (isNothing marble || isNothing cutout)
        (setStatus p "note" "texture decode failed")
      runLoop p $ \t =>
        LIO.run (frame r p stdId holoId ground panel torusM sphereM coneM
                       (groundMat marble) (panelMat cutout) fps t)
          >> reportFps p fps t
