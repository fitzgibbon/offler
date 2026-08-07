#version 300 es
// The engine's line-overlay fragment stage: the object block's lane is the
// RGBA colour.
precision highp float;
out vec4 outColour;
void main() {
  outColour = vec4(pow(lane.rgb, vec3(0.4545)), lane.a);
}
