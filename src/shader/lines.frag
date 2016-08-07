{{GLSL_VERSION}}
{{GLSL_EXTENSIONS}}
{{SUPPORTED_EXTENSIONS}}

#define CIRCLE            0
#define RECTANGLE         1
#define ROUNDED_RECTANGLE 2
#define DISTANCEFIELD     3

in vec4 f_color;
in vec2 f_aa_in_percent;
in vec2 f_uv;
flat in uvec2 f_id;
uniform int shape;
uniform bool dotted;
{{pattern_type}} pattern;

const float ALIASING_CONST = 0.7710678118654757;

float aastep(float threshold1, float value);
float aastep(float threshold1, float threshold2, float value);

void write2framebuffer(vec4 color, uvec2 id);

// x/y pattern
float get_sd(sampler2D pattern, vec2 uv){
    return texture(tex, uv).x;
}
// x pattern
float get_sd(sampler1D pattern, vec2 uv){
    return texture(pattern, uv.x).x;
}
// normal line type
float get_sd(Nothing pattern, vec2 uv){
    vec2 d = max(-uv, uv-vec2(1));
    return -((length(max(vec2(0.0), d)) + min(0.0, max(d.x, d.y))));
}

void main(){
    float alpha = aastep(0, 1, get_sd(pattern, f_uv));
    vec4 color = vec4(f_color.rgb, f_color.a*alpha);
    write2framebuffer(color, f_id);
}
