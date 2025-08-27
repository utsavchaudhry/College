#version 300 es
precision highp float;

in vec3 vN;
in vec3 vE;
in vec3 vL1;
in vec3 vL2;
in vec3 vWorldPos;

uniform vec3  ambientProduct;
uniform vec3  diffuseProduct;   
uniform vec3  specularProduct;
uniform float shininess;
uniform int   uShadingMode;     
uniform float uProcScale;       
uniform float uTime;            
uniform float uWarpAmp;         
uniform int   uOctaves;         

out vec4 fColor;

/*==============================
  Public-domain 3D simplex noise
  Source: Ashima Arts / Stefan Gustavson (public domain)
  https://github.com/ashima/webgl-noise
==============================*/
vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 mod289(vec4 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec4 permute(vec4 x) { return mod289(((x*34.0)+1.0)*x); }
vec4 taylorInvSqrt(vec4 r) { return 1.79284291400159 - 0.85373472095314 * r; }
float snoise(vec3 v)
{
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

  // First corner
  vec3 i  = floor(v + dot(v, C.yyy));
  vec3 x0 = v   - i + dot(i, C.xxx);

  // Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //  x0 = x0 - 0. + 0.0 * C
  vec3 x1 = x0 - i1 + 1.0 * C.xxx;
  vec3 x2 = x0 - i2 + 2.0 * C.xxx;
  vec3 x3 = x0 - 1.0 + 3.0 * C.xxx;

  // Permutations
  i = mod289(i);
  vec4 p = permute( permute( permute(
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

  // Gradients: 7x7 points over a square, mapped onto an octahedron.
  float n_ = 1.0/7.0; //  N=7
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  // mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

  // Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2,p2), dot(p3,p3)));
  p0 *= norm.x; p1 *= norm.y; p2 *= norm.z; p3 *= norm.w;

  // Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                dot(p2,x2), dot(p3,x3) ) );
}

/*==============================
  fBM + domain warping
==============================*/
float fbm(vec3 p, int octaves, float lacunarity, float gain) {
  float a = 0.5;
  float f = 1.0;
  float sum = 0.0;
  for (int i = 0; i < 12; ++i) {     // hard cap for WebGL
    if (i >= octaves) break;
    sum += a * snoise(p * f);
    f *= lacunarity;
    a *= gain;
  }
  return sum;
}

// Domain warp: offset position by a vector noise field
vec3 warp(vec3 p, float amp, float t) {
  // three independent noise lookups with animated offsets
  vec3 q = vec3(
    snoise(p + vec3( 1.7 + 0.07*t, 9.2,  3.1)),
    snoise(p + vec3(-4.1,          2.0 + 0.11*t, -1.6)),
    snoise(p + vec3( 2.3,         -7.3,  5.9 + 0.09*t))
  );
  return p + amp * q;
}

/*==============================
  Cosine color palette (IQ)
  pal(t) = a + b*cos(2π*(c*t + d))
==============================*/
vec3 palette(float t, vec3 a, vec3 b, vec3 c, vec3 d) {
  const float TWO_PI = 6.28318530718;
  return a + b * cos(TWO_PI * (c * t + d));
}

void main() {
  // ---- Animated, asymmetric fractal field ----
  float lacunarity = 2.0;
  float gain       = 0.5;

  // Base coordinate in texture space
  vec3 p = vWorldPos * max(uProcScale, 1e-6);

  // Time-varying domain warp, then fBM
  vec3 pw = warp(p, uWarpAmp, uTime);
  float f = fbm(pw, uOctaves, lacunarity, gain);

  // Normalize f to [0,1]-ish for coloring (fBM ~ [-1,1])
  float t = 0.5 + 0.5 * f;

  // Vibrant cosine palette (tweaked for asymmetry and color richness)
  vec3 kd = palette(
    t,
    vec3(0.50, 0.50, 0.50),  // a: base
    vec3(0.50, 0.50, 0.50),  // b: contrast
    vec3(1.00, 1.40, 1.70),  // c: frequency per channel
    vec3(0.00, 0.15, 0.30)   // d: phase per channel
  );

  // ---- Phong lighting (camera space) ----
  vec3 N  = normalize(vN);
  vec3 E  = normalize(vE);
  vec3 L1 = normalize(vL1);
  vec3 L2 = normalize(vL2);

  vec3 ambient = ambientProduct;

  float d1 = max(dot(L1, N), 0.0);
  float d2 = max(dot(L2, N), 0.0);
  vec3 diffuse = (d1 + d2) * diffuseProduct * kd;

  vec3 spec = vec3(0.0);
  if (d1 > 0.0) {
    vec3 H1 = normalize(L1 + E);
    spec += pow(max(dot(N, H1), 0.0), shininess) * specularProduct;
  }
  if (d2 > 0.0) {
    vec3 H2 = normalize(L2 + E);
    spec += pow(max(dot(N, H2), 0.0), shininess) * specularProduct;
  }

  vec3 color = ambient + diffuse + spec;
  fColor = vec4(clamp(color, 0.0, 1.0), 1.0);
}
