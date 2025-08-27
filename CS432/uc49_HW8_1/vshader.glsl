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

uniform int  uOverlayMode;   // 0 = lit surface (default), 1 = unlit solid color
uniform float uPointSize;

in vec3 aPosition;
in vec3 aNormal;

out vec3 vColor;
out vec3 vN;
out vec3 vL1;
out vec3 vL2;
out vec3 vE;

in vec2 aTexCoord;
out vec2 vTexCoord;

void main() {
    // Transform to eye space
    vec3 pos = (modelViewMatrix * vec4(aPosition, 1.0)).xyz;
    vec3 N   = normalize((modelViewMatrix * vec4(aNormal, 0.0)).xyz);
    vec3 E   = normalize(-pos);
    vec3 L1  = normalize(lightPosA - pos);
    vec3 L2  = normalize(lightPosB - pos);

    // flip normal if facing away from eye >>>
    if (dot(N, E) < 0.0) {
        N = -N;
    }

    // Gouraud lighting at vertex
    vec3 ambient = ambientProduct;

    float dTerm1 = max(dot(L1, N), 0.0);
    vec3  diffuse1  = dTerm1 * diffuseProduct;
    vec3  specular1 = vec3(0.0);
    if (dTerm1 > 0.0) {
        vec3 H1 = normalize(L1 + E);
        float sTerm1 = pow(max(dot(N, H1), 0.0), shininess);
        specular1 = sTerm1 * specularProduct;
    }

    float dTerm2 = max(dot(L2, N), 0.0);
    vec3  diffuse2  = dTerm2 * diffuseProduct;
    vec3  specular2 = vec3(0.0);
    if (dTerm2 > 0.0) {
        vec3 H2 = normalize(L2 + E);
        float sTerm2 = pow(max(dot(N, H2), 0.0), shininess);
        specular2 = sTerm2 * specularProduct;
    }

    vec3 color = ambient + diffuse1 + specular1 + diffuse2 + specular2;
    vColor = min(color, vec3(1.0));

    // Pass flipped N so Phong path benefits too
    vN = N;
    vL1 = L1;
    vL2 = L2;
    vE = E;

    vTexCoord = aTexCoord;

    gl_PointSize = uPointSize;

    gl_Position = projectionMatrix * vec4(pos, 1.0);
}
