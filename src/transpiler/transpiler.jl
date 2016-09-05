
immutable FunctionSignature
    name::String
    argtypes::Vector{DataType}
    returntype::DataType
end

immutable Backend{Identifier}
    typemap::Dict{DataType, String} #
    funcnames::Dict{Function, Dict{Pair, String}}
end

function Backend(sym::Symbol)
    Backend{sym}(
        Dict{DataType, String}(),
        Dict{Function, Dict{Pair, String}}()
    )
end

"""
Most backends don't support type parameters, so we need to embedd them in the name
"""
function to_type_name(T)
    join((a.name.name, a.parameters...), "_")
end

function to_type_name{T}(backend, ::Type{T})
    if haskey(backend.typemap, T)
        return backend.typemap[T]
    else
        _declare_composite_type(backend, T)
    end
end

function FunctionSignature(backend, f, types::Tuple{Vararg{DataType}})
    signature = get!(backend.funcnames, (fun2name(f), types)) do

    end
    get!(backend.declared_functions, signature) do

        return declared_functions.typemap[T]
    else
        _declare_function(backend, f, types::Tuple)
    end
end
function to_type_assert(backend, slot, typ)
    error("not implemented for $backend")
end

function _declare_function(backend, f, types)
    error("not implemented for $backend")
end
function declare_function(backend, name, args, returntype, body)
    error("not implemented for $backend")
end
function _declare_composite_type{T}(backend, ::Type{T})
    name = declare_composite_type(backend, T)
    backend.declared_types[T] = name
    name
end
function declare_composite_type(backend, ::Type{T})
    error("not implemented for $backend")
end
