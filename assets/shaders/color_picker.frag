#version 440 core
out vec4 FragColor;  

uniform int color;

void main()
{
    if (color == 1) { 
        FragColor = vec4(0.114f, 0.169f, 0.325f, 1.0f);
    } else if (color == 2) {
        FragColor = vec4(0.494f, 0.145f, 0.325f, 1.0f);
    } else if (color == 3) {
        FragColor = vec4(0.0f, 0.529f, 0.318f, 1.0f);
    } else if (color == 4) {
        FragColor = vec4(0.671f, 0.322f, 0.212f, 1.0f);
    } else if (color == 5) {
        FragColor = vec4(0.373f, 0.341f, 0.310f, 1.0f);
    } else if (color == 6) {
        FragColor = vec4(0.761f, 0.765f, 0.780f, 1.0f);
    } else if (color == 7) {
        FragColor = vec4(1.0f, 0.945f, 0.909f, 1.0f);
    } else if (color == 8) {
        FragColor = vec4(1.0f, 0.0f, 0.302f, 1.0f);
    } else if (color == 9) {
        FragColor = vec4(1.0f, 0.639f, 0.0f, 1.0f);
    } else if (color == 10) {
        FragColor = vec4(1.0f, 0.925f, 0.153f, 1.0f);
    }else if (color == 11) {
        FragColor = vec4(0.0f, 0.894f, 0.212f, 1.0f);
    }else if (color == 12) {
        FragColor = vec4(0.161f, 0.678f, 1.0f, 1.0f);
    }else if (color == 13) {
        FragColor = vec4(0.514f, 0.463f, 0.612f, 1.0f);
    }else if (color == 14) {
        FragColor = vec4(1.0f, 0.467f, 0.659f, 1.0f);
    }else if (color == 15) {
        FragColor = vec4(1.0f, 0.8f, 0.667f, 1.0f);
    } else { // default to black
        FragColor = vec4(0.0f, 0.0f, 0.0f, 1.0f);
    }
}