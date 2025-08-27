#version 300 es
in vec3 aColor;
in vec2 aPosition;
out vec4 vColor;

void main()
{
    gl_Position = vec4(aPosition, 0.0, 1.0);
    vColor = vec4(aColor, 1.0);
}