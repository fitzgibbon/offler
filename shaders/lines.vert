#version 300 es
// The engine's gizmo-overlay vertex stage; attribute and block declarations
// are generated and spliced in after the version line.
out vec4 vColor;
void main() {
  vColor = color * lane;
  gl_Position = proj * view * model * vec4(pos.xyz, 1.0);
}
