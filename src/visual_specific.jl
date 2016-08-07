
function visual_specific!{T<:Point}(
        v::Visualization{Union{VecTypes{T}, MatTypes{T}}, :lines}
    )
    data = copy(v.parameters)
    convert2gl!(GLBuffer, data)
    @gen_defaults! data begin
        is_fully_opaque = false
        transparent_picking = false
        max_primitives = const_lift(length, p_vec)
    end
    if v[:dotted]
        @gen_defaults! data begin
            lastlen = const_lift(sumlengths, p_vec)
            maxlength = const_lift(last, lastlen)
        end
    end
    shaders = ("util.vert", "lines.vert", "lines.geom", "lines.frag")
    assemble(data, shaders, GL_LINE_STRIP_ADJACENCY)
end

function visual_specific!{Pos}(
        v::Visualization{Pos, :speed}
    )
    GLBuffer
    gl_primitive = GL_POINTS
    prerender = ()->glPointSize(scale)
    shaders = ("dots.vert", "dots.frag")
end

function visual_specific!{P<:Primitives3D, Pos, S}(
        v::Visualization{Tuple{P, Pos}, S}
    )
    TextureBuffer
    shaders = ("util.vert", "particles.vert", "standard.frag")
end

function visual_specific!{P<:Sprites, Pos, S}(
        v::Visualization{Tuple{P, Pos}, S},
    )
    GLBuffer
    is_fully_opaque = false
    gl_primitive = GL_POINTS
    shaders = ("util.vert", "sprites.vert", "sprites.geom", "distance_shape.frag")
end
function visual_specific!{T<:Point}(
        v::Visualization{VecTypes{T}, :line_segment}
    )
    gl_primitive = GL_LINES
    is_fully_opaque = false
    transparent_picking = false
    shaders = ("util.vert", "line_segment.vert", "line_segment.geom", "lines.frag")
    max_primitives = const_lift(length, p_vec)
end
