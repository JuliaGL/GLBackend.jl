{{GLSL_VERSION}}
{{GLSL_EXTENSIONS}}

struct Grid1D{
    float minimum;
    float maximum;
    int dims;
    float multiplicator;
};
struct Grid2D{
    vec2 minimum;
    vec2 maximum;
    ivec2 dims;
    vec2 multiplicator;

};
struct Grid3D{
    vec3 minimum;
    vec3 maximum;
    ivec3 dims;
    vec3 multiplicator;
};
struct Vertex1{
    vec3 position;
    vec3 normal;
    vec3 color;
}
struct Vertex2{
    vec3 position;
    vec3 color;
}
struct Vertex3{
    vec3 position;
}

{{mesh_types}}

{{position_types}}

{{rotation_types}}
{{scale_types}}
{{color_types}}

void render(Vertex1 mesh, mat4 viewmodel);
void render(Vertex2 mesh, mat4 viewmodel);
void render(Vertex3 mesh, mat4 viewmodel);

void main(){
    int index = gl_InstanceID;
    o_id = uvec2(objectid, index+1);
    mat4 particle_translation;
    {{position_calc}}
    {{scale_calculation}}
    {{rotation_calculation}}
    {{transform_model}}
    render({{mesh_variables}}, model);
}
