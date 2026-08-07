#version 300 es
// The standard material's vertex stage. The attribute declarations and the
// std140 blocks (Globals, Obj, Mat -- flat members: proj, view, model,
// lane, baseColor, ...) are generated from Offler.Gfx.Layout and spliced in
// after the version directive, which GLSL insists comes first.
out vec3 vNormal;
out vec3 vWorld;
out vec3 vObj;
out vec2 vUv;
out vec4 vICol;
void main() {
  vICol = vec4(1.0);
  // Object-space position, for procedural patterns that must stay welded to
  // the surface as the object moves and spins.
  vObj = pos;
  vec4 world = model * vec4(pos, 1.0);
  vWorld = world.xyz;
  // Approximate for non-uniform scale; exact for rigid-plus-uniform.
  vNormal = mat3(model) * normal;
  vUv = uv;
  gl_Position = proj * view * world;
}
