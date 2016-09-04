
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

fun2name(fun::AbstractString) = fun #LOL!
fun2name{T}(fun::T) = T.name.mt.name #LOL (╯°□°）╯︵ ┻━┻

immutable FuncExpr
    name::Symbol    
    slots
    args
    body::Vector
    returntype::DataType
end

function FuncExpr(f, types)
    lam_lowered = code_lowered(f, types)[1]
    lam_typed = code_typed(f, types)[1]
    slotnames = Base.lambdainfo_slotnames(lam_typed)
    slottypes = lam_typed.slottypes
    nargs = lam_typed.nargs
    slots = [(SlotNumber(i), (name, slottypes[i])) for (i, name) in enumerate(slotnames)]
    ast = Base.uncompressed_ast(lam_lowered)
    FuncExpr(fun2name(f), slots, view(slots, 2:nargs), ast, lam_typed.rettype)
end

function transpile(backend, f::Symbol, types::Tuple)
    func = getfield(current_module(), f)
    transpile(backend, f, types)
end
function transpile(backend, f::Function, types::Tuple)
    func = FuncExpr(f, types)
    empty!(backend.current_slots)
    merge!(backend.current_slots, Dict(func.slots))
    declare_function(backend, func)
end

function transpile(backend, block::Expr)
    if block.head == :block || block.head == :body
        transpile(backend, block.args)
    else
        # I knoow, Val no good here... But it makes the function overwritable and composable, which might be nice
        # in order to have unspecialized fall backs which can be specialized
        return transpile(backend, block, Val{block.head}())
    end
end
function transpile(backend::GL, list::AbstractVector)
    lines = map(list) do elem
        transpile(backend, elem)
    end
    block_source = map(lines) do line
        declare_block(backend, source(line))
    end
    return TExpr(join(block_source, ""), returntype(last(lines)))
end