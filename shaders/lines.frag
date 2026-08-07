#version 300 es
// The engine's gizmo-overlay fragment stage: per-vertex colour, lane-tinted.
precision highp float;
in vec4 vColor;
out vec4 outColour;
void main() {
  outColour = vec4(pow(vColor.rgb, vec3(0.4545)), vColor.a);
}
