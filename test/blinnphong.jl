using FixedSizeArrays, GeometryTypes
include(Pkg.dir("GLBackend", "src", "transpiler", "gl_transpiler.jl"))
immutable Light{T}
    position::Vec{3,T}
    ambient::Vec{3,T}
    diffuse::Vec{3,T}
    diffuse_power::T
    specular::Vec{3,T}
    specular_power::T
end

immutable Shading{T}
    ambient::Vec{3, T}
    specular::Vec{3, T}
    shininess::T
end
function blinnphong2{NV, T}(light, L::Vec{NV, T}, N, V, color, shading)
    diff_coeff = max(dot(L,N), T(0.0f0))
    # specular coefficient
    H = normalize(L+V)

    spec_coeff = max(dot(H,N), T(0.0f0))^(shading.shininess)
    if diff_coeff <= T(0.0f0)
        spec_coeff = T(0.0f0)
    elseif diff_coeff <= T(0.2f0)
        # some nonesense to test elseif
        spec_coeff *= T(2.0f0);
        spec_coeff += T(1.0f0);
    else
        spec_coeff = spec_coeff;
        return L
    end
    # final lighting model
    return Vec3f0(
        light.ambient .* shading.ambient +
        light.diffuse .* light.diffuse_power .* color * diff_coeff +
        light.specular .* light.specular_power .* shading.specular * spec_coeff
    )
end
l = Light(Vec3f0(0), Vec3f0(0), Vec3f0(0), 1f0, Vec3f0(0), 1f0)
s = Shading(Vec3f0(0), Vec3f0(0), 1f0)
blinnphong(l, Vec3f0(0), Vec3f0(0), Vec3f0(0), Vec3f0(0), s)
transpile(gl_backend, blinnphong2, (Light{Float32},Vec3f0, Vec3f0, Vec3f0, Vec3f0,Shading{Float32}))



#delete!(GLBackend.function_map, ("blinnphong3", [Light{Float32},Vec3f0, Vec3f0, Vec3f0, Vec3f0,Shading{Float32}]))
