#version 300 es
in vec3 aColor;
in vec3 aPosition;
uniform vec3 translation;
uniform vec3 rotation;
uniform vec3 scale;
out vec4 vColor;

mat4 BuildTranslation(vec3 delta) {
    return mat4(
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      delta.x, delta.y, delta.z, 1.0
    );
}

mat4 BuildRotation(vec3 angles) {
    float cx = cos(angles.x), sx = sin(angles.x);
    float cy = cos(angles.y), sy = sin(angles.y);
    float cz = cos(angles.z), sz = sin(angles.z);

    mat4 Rx = mat4(
        1.0,  0.0,  0.0,  0.0,
        0.0,   cx,  -sx,  0.0,
        0.0,   sx,   cx,  0.0,
        0.0,  0.0,  0.0,  1.0
    );
    mat4 Ry = mat4(
         cy,  0.0,   sy,  0.0,
        0.0,  1.0,  0.0,  0.0,
        -sy,  0.0,   cy,  0.0,
        0.0,  0.0,  0.0,  1.0
    );
    mat4 Rz = mat4(
         cz,  -sz,  0.0,  0.0,
         sz,   cz,  0.0,  0.0,
        0.0,  0.0,  1.0,  0.0,
        0.0,  0.0,  0.0,  1.0
    );
    return Rx * Ry * Rz;
}

mat4 BuildScale(vec3 s) {
    return mat4(
        s.x,  0.0,  0.0,  0.0,
        0.0,  s.y,  0.0,  0.0,
        0.0,  0.0,  s.z,  0.0,
        0.0,  0.0,  0.0,  1.0
    );
}

void main() {
    mat4 T = BuildTranslation(translation);
    mat4 R = BuildRotation(rotation);
    mat4 S = BuildScale(scale);

    gl_Position = T * R * S * vec4(aPosition, 1.0);
    vColor       = vec4(aColor, 1.0);
}
