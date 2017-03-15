{{GLSL_VERSION}}
{{GLSL_EXTENSIONS}}

{{vertex_type}} position;
in float lastlen;

{{color_type}}      color;
{{color_map_type}}  color_map;
{{intensity_type}}  intensity;
{{color_norm_type}} color_norm;

vec4 _color(vec3 color, Nothing intensity, Nothing color_map, Nothing color_norm, int index);
vec4 _color(vec4 color, Nothing intensity, Nothing color_map, Nothing color_norm, int index);
vec4 _color(Nothing color, float intensity, sampler1D color_map, vec2 color_norm, int index);

uniform mat4 projection, view, model;
uniform uint objectid;
uniform ivec2 dims;

out uvec2 g_id;
out vec4 g_color;
out float g_lastlen;
out uint g_line_connections;

vec4 getindex(sampler2D tex, int index);
vec4 getindex(sampler1D tex, int index);

vec4 to_vec4(vec3 v){return vec4(v, 1);}
vec4 to_vec4(vec2 v){return vec4(v, 0, 1);}


void main()
{
    g_lastlen = lastlen;
    int index = gl_VertexID;
    g_id = uvec2(objectid, index+1);
    g_color = _color(color, intensity, color_map, color_norm, index);
    g_line_connections = uint(index/dims.x);
    gl_Position = projection*view*model*to_vec4(position);
}
