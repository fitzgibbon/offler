#version 300 es
// The standard material's fragment stage. The std140 blocks, the
// t_base_color sampler and the offlerAlpha helper are generated and spliced
// in after the version line. Mat's members, flat in scope: baseColor,
// emissive (rgb + unlit flag), params (metallic, roughness, pattern,
// patternScale).
precision highp float;
in vec3 vNormal;
in vec3 vWorld;
in vec3 vObj;
in vec2 vUv;
in vec4 vICol;
out vec4 outColour;

float hash(vec3 p) {
  return fract(sin(dot(p, vec3(127.1, 311.7, 74.7))) * 43758.5453123);
}

float noise(vec3 p) {
  vec3 i = floor(p);
  vec3 f = fract(p);
  f = f * f * (3.0 - 2.0 * f);
  float n000 = hash(i + vec3(0,0,0)), n100 = hash(i + vec3(1,0,0));
  float n010 = hash(i + vec3(0,1,0)), n110 = hash(i + vec3(1,1,0));
  float n001 = hash(i + vec3(0,0,1)), n101 = hash(i + vec3(1,0,1));
  float n011 = hash(i + vec3(0,1,1)), n111 = hash(i + vec3(1,1,1));
  return mix(mix(mix(n000, n100, f.x), mix(n010, n110, f.x), f.y),
             mix(mix(n001, n101, f.x), mix(n011, n111, f.x), f.y), f.z);
}

float fbm(vec3 p) {
  float total = 0.0, amp = 0.5;
  for (int k = 0; k < 4; k++) {
    total += amp * noise(p);
    p *= 2.03;
    amp *= 0.5;
  }
  return total;
}

// Procedural modulation of the base colour, over object space so it sticks
// to the surface. 0 plain, 1 checker, 2 value-noise fbm.
float patternFactor() {
  if (params.z < 0.5) return 1.0;
  vec3 p = vObj * params.w;
  if (params.z < 1.5) {
    vec3 q = floor(p);
    return mix(0.4, 1.0, mod(q.x + q.y + q.z, 2.0));
  }
  return 0.3 + 1.5 * fbm(p);
}

void main() {
  // The default binding is the renderer's 1x1 white, so an unmapped
  // material multiplies by one.
  vec4 texel = texture(t_base_color, vUv);
  vec4 base4 = baseColor * texel * vICol;
  vec3 base = base4.rgb * patternFactor();

  // Unlit: the (patterned, textured) base colour exactly.
  if (emissive.w > 0.5) {
    outColour = offlerAlpha(vec4(pow(base, vec3(0.4545)), base4.a));
    return;
  }

  vec3 N = normalize(vNormal);
  vec3 V = normalize(cam - vWorld);

  float metallic = params.x;
  float rough = clamp(params.y, 0.03, 1.0);
  // Perceptual roughness to a Blinn-Phong exponent: matte 2, mirror ~1400.
  float shininess = exp2(1.0 + 9.5 * (1.0 - rough));
  float specScale = 1.0 - 0.6 * rough;

  // counts = (light count, ambient, -, -); up to maxLights directionals.
  vec3 diffAcc = vec3(0.0);
  vec3 specAcc = vec3(0.0);
  int count = int(counts.x);
  for (int i = 0; i < 4; i++) {
    if (i >= count) break;
    vec3 L = normalize(-lightDirs[i].xyz);
    vec3 H = normalize(L + V);
    vec3 lc = lightColors[i].rgb;
    diffAcc += max(dot(N, L), 0.0) * lc;
    specAcc += pow(max(dot(N, H), 0.0), shininess) * specScale * lc;
  }

  vec3 specTint = mix(vec3(1.0), base, metallic);
  vec3 diffuse = base * (1.0 - 0.9 * metallic);

  vec3 colour = diffuse * (vec3(counts.y) + diffAcc)
              + specTint * specAcc
              + emissive.rgb;

  outColour = offlerAlpha(vec4(pow(colour, vec3(0.4545)), base4.a));
}
