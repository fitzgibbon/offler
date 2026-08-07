#version 300 es
// The standard material's instanced vertex stage: im0..im3 (the instance
// model matrix's columns) and icolor stream at instance rate; model is the
// whole batch's transform. Attribute and block declarations are generated
// and spliced in after the version line.
out vec3 vNormal;
out vec3 vWorld;
out vec3 vObj;
out vec2 vUv;
out vec4 vICol;
void main() {
  mat4 im = model * mat4(im0, im1, im2, im3);
  vObj = pos;
  vec4 world = im * vec4(pos, 1.0);
  vWorld = world.xyz;
  vNormal = mat3(im) * normal;
  vUv = uv;
  vICol = icolor;
  gl_Position = proj * view * world;
}
