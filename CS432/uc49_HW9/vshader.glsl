#version 300 es
precision mediump float;
precision mediump int;

uniform mat4 modelViewMatrix;
uniform mat4 projectionMatrix;
uniform vec3 lightPosA;
uniform vec3 lightPosB;
uniform vec3 ambientProduct;
uniform vec3 diffuseProduct;
uniform vec3 specularProduct;
uniform float shininess;

in vec3 aPosition;
in vec3 aNormal;

out vec3 vColor;
out vec3 vN;
out vec3 vL1;
out vec3 vL2;
out vec3 vE;

void main() {
    // Transform vertex position and normal to eye coordinates
    vec3 pos = (modelViewMatrix * vec4(aPosition, 1.0)).xyz;
    vec3 N   = normalize((modelViewMatrix * vec4(aNormal, 0.0)).xyz);
    vec3 E   = normalize(-pos);
    vec3 L1_vec = lightPosA - pos;
    vec3 L2_vec = lightPosB - pos;
    vec3 L1 = normalize(L1_vec);
    vec3 L2 = normalize(L2_vec);

    // Compute Phong illumination at the vertex (Gouraud shading)
    vec3 ambient = ambientProduct;
    // Light A contribution
    float dTerm1 = max(dot(L1, N), 0.0);
    vec3 diffuse1 = dTerm1 * diffuseProduct;
    vec3 specular1 = vec3(0.0);
    if (dTerm1 > 0.0) {
        vec3 H1 = normalize(L1 + E);
        float sTerm1 = pow(max(dot(N, H1), 0.0), shininess);
        specular1 = sTerm1 * specularProduct;
    }
    // Light B contribution
    float dTerm2 = max(dot(L2, N), 0.0);
    vec3 diffuse2 = dTerm2 * diffuseProduct;
    vec3 specular2 = vec3(0.0);
    if (dTerm2 > 0.0) {
        vec3 H2 = normalize(L2 + E);
        float sTerm2 = pow(max(dot(N, H2), 0.0), shininess);
        specular2 = sTerm2 * specularProduct;
    }
    vec3 color = ambient + diffuse1 + specular1 + diffuse2 + specular2;
    vColor = min(color, vec3(1.0));  // clamp each component to 1.0

    // Pass through vectors for Phong shading in fragment
    vN = N;
    vL1 = L1;
    vL2 = L2;
    vE = E;

    gl_Position = projectionMatrix * vec4(pos, 1.0);
}
