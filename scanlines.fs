#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;   // Texture passed in by Raylib
uniform float lines;          // Number of vertical lines (e.g. 200)

void main()
{
    vec4 color = texture(texture0, fragTexCoord);

    // Map fragTexCoord.y [0..1] to [0..lines]
    float y = fragTexCoord.y * lines;

    // If even line, darken
    if (int(floor(y)) % 2 == 0) {
        color.rgb *= 0.85;
    }

    finalColor = color;
}
