#version 300 es
precision highp float;

uniform mat4 uModelView;
uniform mat4 uProjection;

in vec4 aPosition;
in vec3 aColor;
flat out vec3 vColor;

void main() {
    vColor = aColor;
    gl_Position = uProjection * uModelView * aPosition;
}
