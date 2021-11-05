#version 330 core

in vec2 o_texcoords;

uniform sampler2D u_screen;

void main() {
    float x = texture2D(u_screen, vec2(o_texcoords.x, 1.0 - o_texcoords.y)).x;
    gl_FragColor = vec4(x, x, x, 1.0);
}
