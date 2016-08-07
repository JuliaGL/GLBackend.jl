{{GLSL_VERSION}}
#define NUMBER_OF_LIGHTS = {{NUMBER_OF_LIGHTS}};

struct Light{
    vec3 position;
    vec3 diffuse;
    float diffuse_power;
    vec3 specular;
    float specular_power;
};
struct Shading{
    vec3 ambient;
    vec3 specular;
    float shininess;
};

uniform vec3 ambient_color;
uniform Shading shading;

in vec3 o_normal;
in Light o_lights[NUMBER_OF_LIGHTS];
in vec3 o_vertex;
in vec4 o_color;
flat in uvec2 o_id;


vec3 blinnphong(Light light, vec3 L, vec3 N, vec3 V, vec3 color)
{
    float diff_coeff = max(dot(L,N), 0.0);

    // specular coefficient
    vec3 H = normalize(L+V);

    float spec_coeff = pow(max(dot(H,N), 0.0), shading.shininess);
    if (diff_coeff <= 0.0)
        spec_coeff = 0.0;

    // final lighting model
    return vec3(
        light.ambient * shading.ambient +
        light.color * light.diffuse_power * color * diff_coeff +
        light.specular * light.specular_power * shading.specular * spec_coeff
    );
}

void write2framebuffer(vec4 color, uvec2 id);

void main(){
    vec3 N = normalize(o_normal);
    vec3 final_color = vec3(0);
    int i = 0;
    vec3 L;
    for(i;i<NUMBER_OF_LIGHTS;i++){
        L = normalize(o_lights[i].position);
        final_color += blinnphong(o_lights[i], L, N, o_vertex, o_color.rgb)
    }
    write2framebuffer(
        vec4(final_color, o_color.a),
        o_id
    );
}
