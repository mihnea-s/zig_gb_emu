#version 330 core

layout (location = 0) in vec2 a_vertex;

out vec2 o_texcoords;

void main() {
    gl_Position = vec4(a_vertex, 0.0, 1.0);
    o_texcoords = (a_vertex.xy + 1.0) / 2.0;
}
