#version 300 es
precision mediump float;
precision mediump int;

uniform vec3 ambientProduct;
uniform vec3 diffuseProduct;
uniform vec3 specularProduct;
uniform float shininess;
uniform int uShadingMode;

uniform int  uOverlayMode;   // 0 = lit, 1 = solid color overlay
uniform vec3 uSolidColor;

in vec3 vColor;
in vec3 vN;
in vec3 vL1;
in vec3 vL2;
in vec3 vE;

out vec4 fColor;

uniform sampler2D uTexture;
in vec2 vTexCoord;

void main() {
    if (uOverlayMode == 1) {
        fColor = vec4(uSolidColor, 1.0);
        return;
    }
    if (uShadingMode == 0) {
        fColor = vec4(vColor, 1.0);
    } else {
        
        vec3 N = normalize(vN);
        vec3 E = normalize(vE);
        vec3 L1 = normalize(vL1);
        vec3 L2 = normalize(vL2);

        vec3 texColor = texture(uTexture, vTexCoord).rgb; 

        vec3 lightDiff = diffuseProduct;  

        vec3 ambient = ambientProduct;  // material ambient (global ambient * material ka)
        float d1 = max(dot(L1, N), 0.0);
        float d2 = max(dot(L2, N), 0.0);

        vec3 diffuse = d1 * lightDiff * texColor + d2 * lightDiff * texColor;
        
        vec3 specular1 = vec3(0.0), specular2 = vec3(0.0);
        if (d1 > 0.0) {
            vec3 H1 = normalize(L1 + E);
            specular1 = pow(max(dot(N, H1), 0.0), shininess) * specularProduct;
        }
        if (d2 > 0.0) {
            vec3 H2 = normalize(L2 + E);
            specular2 = pow(max(dot(N, H2), 0.0), shininess) * specularProduct;
        }
        vec3 color = ambient + diffuse + specular1 + specular2;
        fColor = vec4(min(color, vec3(1.0)), 1.0);
    }
}

