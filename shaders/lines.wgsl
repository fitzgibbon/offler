// The engine's line-overlay shaders. The engine declarations (LineIn,
// Globals g, Obj o) are generated and prepended; the overlay reuses the
// object block's lane as its RGBA colour.

struct LineOut {
  @builtin(position) pos : vec4<f32>,
};

@vertex
fn vs(v : LineIn) -> LineOut {
  var out : LineOut;
  out.pos = g.proj * g.view * o.model * vec4<f32>(v.pos, 1.0);
  return out;
}

@fragment
fn fs() -> @location(0) vec4<f32> {
  return vec4<f32>(pow(o.lane.rgb, vec3<f32>(0.4545)), o.lane.a);
}
