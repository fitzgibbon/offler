#version 300 es
// The engine's line-overlay vertex stage; attribute and block declarations
// are generated and spliced in after the version line.
void main() {
  gl_Position = proj * view * model * vec4(pos, 1.0);
}
