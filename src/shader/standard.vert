{{GLSL_VERSION}}

in vec3 vertices;
in vec3 normals;

uniform vec4 color;
uniform mat4 projection, view, model;
void render(vec3 vertices, vec3 normals, mat4 viewmodel, mat4 projection);

uniform uint objectid;
flat out uvec2 o_id;
out vec4 o_color;

void main()
{
	o_id = uvec2(objectid, gl_VertexID+1);
	o_color = color;
	render(vertices, normals, view*model, projection);
}
