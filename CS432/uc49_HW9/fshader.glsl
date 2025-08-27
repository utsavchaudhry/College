#version 300 es
precision mediump float;
precision mediump int;

uniform vec3 ambientProduct;
uniform vec3 diffuseProduct;
uniform vec3 specularProduct;
uniform float shininess;
uniform int uShadingMode;

uniform int uColorIndex;
const vec3 PICK_COLORS[4] = vec3[](
    vec3(0.0, 0.0, 0.0),   // index 0: no picking (unused color)
    vec3(1.0, 0.0, 0.0),   // index 1: Red
    vec3(0.0, 1.0, 0.0),   // index 2: Green
    vec3(0.0, 0.0, 1.0)    // index 3: Blue
);

in vec3 vColor;
in vec3 vN;
in vec3 vL1;
in vec3 vL2;
in vec3 vE;

out vec4 fColor;

void main() {

    if(uColorIndex != 0) {
        // Picking mode: output the object’s ID color and skip lighting
        fColor = vec4(PICK_COLORS[uColorIndex], 1.0);
        return;
    }

    if (uShadingMode == 0) {
        // Gouraud shading: use interpolated vertex color
        fColor = vec4(vColor, 1.0);
    } else {
        // Phong shading: compute lighting per fragment
        vec3 N = normalize(vN);
        vec3 E = normalize(vE);
        vec3 L1 = normalize(vL1);
        vec3 L2 = normalize(vL2);
        // Ambient term (global ambient light)
        vec3 ambient = ambientProduct;
        // Light A contributions
        float dTerm1 = max(dot(L1, N), 0.0);
        vec3 diffuse1 = dTerm1 * diffuseProduct;
        vec3 specular1 = vec3(0.0);
        if (dTerm1 > 0.0) {
            vec3 H1 = normalize(L1 + E);
            float sTerm1 = pow(max(dot(N, H1), 0.0), shininess);
            specular1 = sTerm1 * specularProduct;
        }
        // Light B contributions
        float dTerm2 = max(dot(L2, N), 0.0);
        vec3 diffuse2 = dTerm2 * diffuseProduct;
        vec3 specular2 = vec3(0.0);
        if (dTerm2 > 0.0) {
            vec3 H2 = normalize(L2 + E);
            float sTerm2 = pow(max(dot(N, H2), 0.0), shininess);
            specular2 = sTerm2 * specularProduct;
        }
        vec3 color = ambient + diffuse1 + specular1 + diffuse2 + specular2;
        fColor = vec4(min(color, vec3(1.0)), 1.0);
    }
}
