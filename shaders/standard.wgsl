// VertexIn, LineIn, Globals, Obj and their @group/@binding declarations are
// generated from Offler.Gfx.Layout and prepended at pipeline creation, so
// nothing here can drift from the offsets the Idris side pokes at or the
// layout the pipeline is built with.

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
  // Approximate for non-uniform scale; exact for rigid-plus-uniform.
  out.nrm = (o.model * vec4<f32>(v.normal, 0.0)).xyz;
  out.pos = g.proj * g.view * world;
  return out;
}

// Lines carry no normal; they are drawn unlit, so any value serves.
@vertex
fn vs_line(v : LineIn) -> VsOut {
  var out : VsOut;
  let world = o.model * vec4<f32>(v.pos, 1.0);
  out.world = world.xyz;
  out.nrm = vec3<f32>(0.0, 1.0, 0.0);
  out.pos = g.proj * g.view * world;
  return out;
}

@fragment
fn fs(in : VsOut) -> @location(0) vec4<f32> {
  // Unlit: the base colour exactly. First, so lines and 2D pay for nothing
  // below.
  if (o.emissive.w > 0.5) {
    return vec4<f32>(pow(o.baseColor.rgb, vec3<f32>(0.4545)), o.baseColor.a);
  }

  let N = normalize(in.nrm);
  let L = normalize(-g.lightDir);
  let V = normalize(g.cam - in.world);
  let H = normalize(L + V);

  let metallic = o.params.x;
  let rough = clamp(o.params.y, 0.03, 1.0);

  let diff = max(dot(N, L), 0.0);
  // Perceptual roughness to a Blinn-Phong exponent: matte 2, mirror ~1400.
  let shininess = exp2(1.0 + 9.5 * (1.0 - rough));
  let spec = pow(max(dot(N, H), 0.0), shininess) * (1.0 - 0.6 * rough);

  let specTint = mix(vec3<f32>(1.0), o.baseColor.rgb, vec3<f32>(metallic));
  let diffuse = o.baseColor.rgb * (1.0 - 0.9 * metallic);

  let colour = diffuse * (vec3<f32>(g.ambient) + diff * g.lightColor)
             + specTint * spec * g.lightColor
             + o.emissive.rgb;

  return vec4<f32>(pow(colour, vec3<f32>(0.4545)), o.baseColor.a);
}
