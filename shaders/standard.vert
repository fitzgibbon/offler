#version 300 es
// The `in` declarations are generated from Offler.Gfx.Layout and spliced in
// after the version directive, which GLSL insists comes first.
uniform mat4 uProj;
uniform mat4 uView;
uniform mat4 uModel;
out vec3 vNormal;
out vec3 vWorld;
void main() {
  vec4 world = uModel * vec4(pos, 1.0);
  vWorld = world.xyz;
  // Approximate for non-uniform scale; exact for the rigid-plus-uniform
  // transforms Offler.Transform composes.
  vNormal = mat3(uModel) * normal;
  gl_Position = uProj * uView * world;
}
