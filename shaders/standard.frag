#version 300 es
precision highp float;
in vec3 vNormal;
in vec3 vWorld;
uniform vec3 uCam;
uniform float uTime;
uniform vec3 uLightDir;
uniform float uAmbient;
uniform vec3 uLightColor;
uniform vec4 uBaseColor;
uniform vec4 uEmissive;   // rgb, w = shading mode
uniform vec4 uParams;     // metallic, roughness
out vec4 outColour;

void main() {
  // Unlit: the base colour exactly. First, so lines and 2D pay for nothing
  // below.
  if (uEmissive.w > 0.5) {
    outColour = vec4(pow(uBaseColor.rgb, vec3(0.4545)), uBaseColor.a);
    return;
  }

  vec3 N = normalize(vNormal);
  vec3 L = normalize(-uLightDir);
  vec3 V = normalize(uCam - vWorld);
  vec3 H = normalize(L + V);

  float metallic = uParams.x;
  float rough = clamp(uParams.y, 0.03, 1.0);

  float diff = max(dot(N, L), 0.0);
  // Perceptual roughness to a Blinn-Phong exponent: matte 2, mirror ~1400.
  float shininess = exp2(1.0 + 9.5 * (1.0 - rough));
  float spec = pow(max(dot(N, H), 0.0), shininess) * (1.0 - 0.6 * rough);

  vec3 specTint = mix(vec3(1.0), uBaseColor.rgb, metallic);
  vec3 diffuse = uBaseColor.rgb * (1.0 - 0.9 * metallic);

  vec3 colour = diffuse * (vec3(uAmbient) + diff * uLightColor)
              + specTint * spec * uLightColor
              + uEmissive.rgb;

  outColour = vec4(pow(colour, vec3(0.4545)), uBaseColor.a);
}
