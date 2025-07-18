#version 330

in vec2 fragTexCoord;
out vec4 finalColor;

uniform sampler2D texture0;
uniform int enableGrayscale;
uniform int enableScanlines;
uniform float lines;          // Number of vertical lines (e.g. 200)

void main() {
    vec4 color = texture(texture0, fragTexCoord);

    if (enableGrayscale != 0) {
        float gray = dot(color.rgb, vec3(0.299, 0.587, 0.114));
        color.rgb = vec3(gray);
    }

    if (enableScanlines != 0) {
        float y = fragTexCoord.y * lines;

        // If even line, darken
        if (int(floor(y)) % 2 == 0) {
            color.rgb *= 0.9;
        }        
    }

    finalColor = color;
}
