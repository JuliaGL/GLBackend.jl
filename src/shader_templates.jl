
function position_calc(x...)
    _position_calc(filter(x->!isa(x, Void), x)...)
end
function glsllinspace(position::Grid, gi, index)
    """
    (((float(position.dims[$gi])-($(index)+1)) *
        position.minimum[$gi] + $(index)*position.maximum[$gi]) *
        position.multiplicator[$gi])
    """
end
function glsllinspace(grid::Grid{1}, gi, index)
    """
    (((float(position.dims)-($(index)+1)) *
        position.minimum + $(s)*position.maximum) *
        position.multiplicator)
    """
end
function grid_pos(grid::Grid{1})
    "$(glsllinspace(grid, 0, "index"))"
end
function grid_pos(grid::Grid{2})
    "vec2($(glsllinspace(grid, 0, "index2D.x")), $(glsllinspace(grid, 1, "index2D.y")))"
end
function grid_pos(grid::Grid{3})
    "vec3(
        $(glsllinspace(grid, 0, "index2D.x")),
        $(glsllinspace(grid, 1, "index2D.y")),
        $(glsllinspace(grid, 2, "index2D.z"))
    )"
end


function _position_calc{T<:AbstractFloat}(
        grid::Grid{2}, position_z::MatTypes{T}, target::Type{Texture}
    )
    """
    ivec2 index2D = ind2sub(position.dims, index);
    vec2 normalized_index = vec2(index2D) / vec2(position.dims);
    float height = texture(position_z, normalized_index+(offset/vec2(position.dims))).x;
    pos = vec3($(grid_pos(grid)), height);
    """
end

function _position_calc{T<:AbstractFloat}(
        position_x::MatTypes{T}, position_y::MatTypes{T}, position_z::MatTypes{T}, target::Type{Texture}
    )
"""
    ivec2 index2D = ind2sub(dims, index);
    vec2 normalized_index = vec2(index2D) / vec2(dims);
    vec2 offsetted_index = normalized_index + (offset/vec2(dims));
    pos = vec3(
        texture(position_x, offsetted_index).x,
        texture(position_y, offsetted_index).x,
        texture(position_z, offsetted_index).x
    );
"""
end

function _position_calc{T<:AbstractFloat}(
        position_x::VecTypes{T}, position_y::T, position_z::T, target::Type{TextureBuffer}
    )
    "pos = vec3(texelFetch(position_x, index).x, position_y, position_z);"
end
function _position_calc{T<:AbstractFloat}(
        position_x::VecTypes{T}, position_y::T, position_z::T, target::Type{GLBuffer}
    )
    "pos = vec3(position_x, position_y, position_z);"
end
function _position_calc{T<:FixedVector}(
        position_xyz::VecTypes{T}, target::Type{TextureBuffer}
    )
    "pos = texelFetch(position, index).xyz;"
end
function _position_calc{T<:FixedVector}(
        position_xyz::VecTypes{T}, target::Type{GLBuffer}
    )
    len = length(T)
    filler = join(ntuple(x->0, 3-len), ", ")
    needs_comma = len != 3 ? ", " : ""
    "pos = vec3(position $needs_comma $filler);"
end
function _position_calc{T<:AbstractFloat}(
        position_x::VecTypes{T}, position_y::VecTypes{T}, position_z::VecTypes{T},
        target::Type{TextureBuffer}
    )
    "pos = vec3(
        texelFetch(position_x, index).x,
        texelFetch(position_y, index).x,
        texelFetch(position_z, index).x
    );"
end
function _position_calc{T<:AbstractFloat}(
        position_x::VecTypes{T}, position_y::VecTypes{T}, position_z::VecTypes{T},
        target::Type{GLBuffer}
    )
    "pos = vec3(
        position_x,
        position_y,
        position_z
    );"
end
function _position_calc(
        position::Grid{1}, target
    )
    "
    pos = vec3($(grid_pos(position)), 0, 0);
    "
end
function _position_calc(
        position::Grid{2}, target
    )
    "
    ivec2 index2D = ind2sub(position.dims, index);
    pos = vec3($(grid_pos(position)), 0);
    "
end
function _position_calc{T}(
        position::Grid{2}, ::VecTypes{T}, target::Type{GLBuffer}
    )
    "
    ivec2 index2D = ind2sub(position.dims, index);
    pos = vec3($(grid_pos(position)), position_z);
    "
end
function _position_calc(
        position::Grid{3}, target
    )
    "
    ivec3 index2D = ind2sub(position.dims, index);
    pos = $(grid_pos(position));
    "
end
