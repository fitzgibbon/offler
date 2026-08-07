// The standard material's authored body. Everything it references --
// VertexIn, Globals g, Obj o, Mat m, t_base_color/s_base_color, offlerAlpha
// -- is generated from Offler.Gfx.Layout and the material's field list and
// prepended at registration, so nothing here can drift from the offsets the
// Idris side pokes at or the layout the pipelines are built with.
//
// Mat, for this material: baseColor (vec4), emissive (rgb + unlit flag),
// params (metallic, roughness, pattern, patternScale).

struct VsOut {
  @builtin(position) pos : vec4<f32>,
  @location(0) nrm   : vec3<f32>,
  @location(1) world : vec3<f32>,
  @location(2) obj   : vec3<f32>,
  @location(3) uv    : vec2<f32>,
};

@vertex
fn vs(v : VertexIn) -> VsOut {
  var out : VsOut;
  let world = o.model * vec4<f32>(v.pos, 1.0);
  out.world = world.xyz;
  // Object-space position, for procedural patterns that must stay welded to
  // the surface as the object moves and spins.
  out.obj = v.pos;
  // Approximate for non-uniform scale; exact for rigid-plus-uniform.
  out.nrm = (o.model * vec4<f32>(v.normal, 0.0)).xyz;
  out.uv = v.uv;
  out.pos = g.proj * g.view * world;
  return out;
}

fn hash(p : vec3<f32>) -> f32 {
  return fract(sin(dot(p, vec3<f32>(127.1, 311.7, 74.7))) * 43758.5453123);
}

fn noise(p : vec3<f32>) -> f32 {
  let i = floor(p);
  var f = fract(p);
  f = f * f * (vec3<f32>(3.0) - 2.0 * f);
  let n000 = hash(i + vec3<f32>(0.0, 0.0, 0.0));
  let n100 = hash(i + vec3<f32>(1.0, 0.0, 0.0));
  let n010 = hash(i + vec3<f32>(0.0, 1.0, 0.0));
  let n110 = hash(i + vec3<f32>(1.0, 1.0, 0.0));
  let n001 = hash(i + vec3<f32>(0.0, 0.0, 1.0));
  let n101 = hash(i + vec3<f32>(1.0, 0.0, 1.0));
  let n011 = hash(i + vec3<f32>(0.0, 1.0, 1.0));
  let n111 = hash(i + vec3<f32>(1.0, 1.0, 1.0));
  return mix(mix(mix(n000, n100, f.x), mix(n010, n110, f.x), f.y),
             mix(mix(n001, n101, f.x), mix(n011, n111, f.x), f.y), f.z);
}

fn fbm(p0 : vec3<f32>) -> f32 {
  var p = p0;
  var total = 0.0;
  var amp = 0.5;
  for (var k = 0; k < 4; k = k + 1) {
    total = total + amp * noise(p);
    p = p * 2.03;
    amp = amp * 0.5;
  }
  return total;
}

// Procedural modulation of the base colour, over object space so it sticks
// to the surface. 0 plain, 1 checker, 2 value-noise fbm.
fn patternFactor(obj : vec3<f32>) -> f32 {
  if (m.params.z < 0.5) { return 1.0; }
  let p = obj * m.params.w;
  if (m.params.z < 1.5) {
    let q = floor(p);
    let ck = (q.x + q.y + q.z) - 2.0 * floor((q.x + q.y + q.z) * 0.5);
    return mix(0.4, 1.0, ck);
  }
  return 0.3 + 1.5 * fbm(p);
}

@fragment
fn fs(in : VsOut) -> @location(0) vec4<f32> {
  // Sampled outside all control flow, as WGSL's uniformity analysis wants.
  // The default binding is the renderer's 1x1 white, so an unmapped
  // material multiplies by one.
  let texel = textureSample(t_base_color, s_base_color, in.uv);
  let base4 = m.baseColor * texel;
  let base = base4.rgb * patternFactor(in.obj);

  // Unlit: the (patterned, textured) base colour exactly. First, so lines
  // of work below are never paid for by 2D.
  if (m.emissive.w > 0.5) {
    return offlerAlpha(vec4<f32>(pow(base, vec3<f32>(0.4545)), base4.a));
  }

  let N = normalize(in.nrm);
  let L = normalize(-g.lightDir);
  let V = normalize(g.cam - in.world);
  let H = normalize(L + V);

  let metallic = m.params.x;
  let rough = clamp(m.params.y, 0.03, 1.0);

  let diff = max(dot(N, L), 0.0);
  // Perceptual roughness to a Blinn-Phong exponent: matte 2, mirror ~1400.
  let shininess = exp2(1.0 + 9.5 * (1.0 - rough));
  let spec = pow(max(dot(N, H), 0.0), shininess) * (1.0 - 0.6 * rough);

  let specTint = mix(vec3<f32>(1.0), base, vec3<f32>(metallic));
  let diffuse = base * (1.0 - 0.9 * metallic);

  let colour = diffuse * (vec3<f32>(g.ambient) + diff * g.lightColor)
             + specTint * spec * g.lightColor
             + m.emissive.rgb;

  return offlerAlpha(vec4<f32>(pow(colour, vec3<f32>(0.4545)), base4.a));
}
