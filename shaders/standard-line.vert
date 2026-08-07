#version 300 es
// The standard material's line-topology vertex stage: position only,
// neutral normal and uv, so line meshes draw as flat (textured-at-origin)
// base colour. Attribute and block declarations are generated and spliced
// in after the version line.
out vec3 vNormal;
out vec3 vWorld;
out vec3 vObj;
out vec2 vUv;
void main() {
  vObj = pos;
  vec4 world = model * vec4(pos, 1.0);
  vWorld = world.xyz;
  vNormal = vec3(0.0, 1.0, 0.0);
  vUv = vec2(0.0, 0.0);
  gl_Position = proj * view * world;
}
