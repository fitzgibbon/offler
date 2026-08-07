// The engine's gizmo-overlay shaders. The engine declarations (LineIn,
// Globals g, Obj o) are generated and prepended. Colour is per vertex,
// multiplied by the object block's lane, which the overlay uses as a
// whole-overlay tint (white when untinted).

struct LineOut {
  @builtin(position) pos : vec4<f32>,
  @location(0) color : vec4<f32>,
};

@vertex
fn vs(v : LineIn) -> LineOut {
  var out : LineOut;
  out.pos = g.proj * g.view * o.model * vec4<f32>(v.pos.xyz, 1.0);
  out.color = v.color * o.lane;
  return out;
}

@fragment
fn fs(in : LineOut) -> @location(0) vec4<f32> {
  return vec4<f32>(pow(in.color.rgb, vec3<f32>(0.4545)), in.color.a);
}
