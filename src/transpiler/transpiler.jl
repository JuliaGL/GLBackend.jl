
immutable FunctionSignature
    name::String
    argtypes::Vector{DataType}
    returntype::DataType
end

immutable Backend{Identifier}
    typemap::Dict{DataType, String} #
    funcnames::Dict{Tuple{String, Vector{DataType}}, FunctionSignature}
    declared_functions::Dict{FunctionSignature, String} # name, arg_types
    declared_types::Dict{String, Any} # typename
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
fun2name{T}(fun::AbstractString) = fun #LOL!
fun2name{T}(fun::T) = T.name.mt.name #LOL!
function fun2expr(f, types)
    lam = code_typed(f, types)[1]
    slotnames = Base.lambdainfo_slotnames(lam)
    slots = [(SlotNumber(i), (name, types[i])) for (i, name) in enumerate(slotnames)]
    args = map(last, slots[2:lam.nargs])
    slot_dict = Dict(slots)
    ast = Base.uncompressed_ast(lam)
    if ast !== nothing
        body.args = ast
        body.typ = l.rettype
    end
    lam, slots
end

function transpile(backend, f::Function, types::Tuple)
    lambda, body = fun2expr(f, types)

    body = transpile(backend, body)
    declare_function(backend, fun2name(f), slots, lambda.rettype, body)
end

function transpile(backend, block::Expr)
    if block.head == :block
        for elem in block.args
            transpile(backend, elem)
        end
    else

    end
end
