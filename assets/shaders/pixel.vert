#version 440 core
layout (location = 0) in vec2 a_pos; // top left corner of fat pixel

// offsetx, y will move to the vertices of the fat pixel
uniform float offsetx;
uniform float offsety;

void main()
{
    gl_Position = vec4(a_pos.x + offsetx, a_pos.y + offsety, 0.0, 1.0);
}