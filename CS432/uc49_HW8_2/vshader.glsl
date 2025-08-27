#version 300 es
precision highp float;

uniform mat4 modelViewMatrix;
uniform mat4 projectionMatrix;

uniform vec3 lightPosA;    // in camera space
uniform vec3 lightPosB;    // in camera space

in vec3 aPosition;
in vec3 aNormal;

out vec3 vN;       // normal in camera space
out vec3 vE;       // eye vector in camera space (from point to eye)
out vec3 vL1;      // light A vector in camera space (from point to light)
out vec3 vL2;      // light B vector in camera space
out vec3 vWorldPos; // object/world space position (anchors 3D procedural texture)

void main() {
    
    vWorldPos = aPosition;
    
    vec3 posEye = (modelViewMatrix * vec4(aPosition, 1.0)).xyz;
    
    vN = mat3(modelViewMatrix) * aNormal;

    vE  = -posEye;                // eye at (0,0,0) in camera space
    vL1 = lightPosA - posEye;     // light A (camera space)
    vL2 = lightPosB - posEye;     // light B (camera space)

    gl_Position = projectionMatrix * vec4(posEye, 1.0);
}
