#version 300 es
precision highp float;

flat in vec3 vColor;
out vec4 fColor;

void main() {
    fColor = vec4(vColor, 1.0);
}
