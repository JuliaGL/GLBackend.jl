#3D primitives
typealias Primitives3D  Union{AbstractGeometry{3}, AbstractMesh}
#2D primitives AKA sprites, since they are shapes mapped onto a 2D rectangle
typealias Sprites Union{AbstractGeometry{2}, Shape, Char, Type}
typealias AllPrimitives Union{AbstractGeometry, Shape, Char}



function assemble(data, shaders, primitive, instanced=nothing)
    if instanced == nothing
    end

end

function convert2gl!(buffer_preference, dict)
    view = Dict{String, String}()
    map!(dict) do key_value
        k,v = key_value
        new_val = if isa(v, AbstractArray)
            gl_convert(buffer_preference, v)
        elseif isa(v, AbstractMesh)
            NativeMesh(v)
        else
            gl_convert(v)
        end
        return k => new_val
    end

end
