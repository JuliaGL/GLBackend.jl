include("goto_remap.jl")
include("lambdas.jl")

"""
Most backends don't support type parameters, so we need to embedd them in the name
"""
function to_type_name(T)
    join((T.name.name, T.parameters...), "_")
end
#
# function to_type_name{T}(backend, ::Type{T})
#     if haskey(backend.typemap, T)
#         return backend.typemap[T]
#     else
#         _declare_composite_type(backend, T)
#     end
# end
# function _declare_composite_type{T}(backend, ::Type{T})
#     name = declare_composite_type(backend, T)
#     backend.declared_types[T] = name
#     name
# end
function to_type_assert(backend, slot, typ)
    error("not implemented for $backend")
end

function _declare_function(backend, f, types)
    error("not implemented for $backend")
end
function declare_function(backend, name, args, returntype, body)
    error("not implemented for $backend")
end

function declare_composite_type(backend, ::Type)
    error("not implemented for $backend")
end
abstract TranspilerBackend


function transpile(backend::TranspilerBackend, block::Expr)
    if block.head == :block || block.head == :body
        transpile(backend, block.args)
    else
        # I knoow, Val no good here... But it makes the function overwritable and composable, which might be nice
        # in order to have unspecialized fall backs which can be specialized
        #println("will transpile: $(block.head) ", block)
        return transpile(backend, block, Val{block.head}())
    end
end

function transpile(backend::TranspilerBackend, list::AbstractVector)
    last_type = Any
    block_source = ""
    state = start(list)
    while !done(list, state)
        elem, state = next(list, state)
        local line::Union{TExpr, Void}
        try
            line = transpile(backend, elem)
        catch e
            println("Error at:")
            println("current slots: ")
            for (slot, (n,t)) in last(backend.current_slots)
                println("   ", slot, "->", n, " ", t)
            end
            for x in list[1:state-1]
                println("   ", x)
            end
            println("--> ", elem)

            rethrow(e)
        end
        line == nothing && continue
        block_source *= declare_block(backend, source(line))
        last_type = returntype(line)
    end
    return TExpr(block_source, last_type)
end
transpile(backend::TranspilerBackend, expr::Void) = nothing



# Utils

isbox(x) = (x == GlobalRef(Base, :box))
extract_sym(T, x) = error("$x $(typeof(x)) cant be converted to Symbol")
extract_sym{T}(::Type{T}, x::Int) = make_name(fieldname(T, x))
extract_sym(T, x::Symbol) = x
extract_sym(T, x::QuoteNode) = x.value
extract_sym(T, x) = error("$x $(typeof(x)) cant be converted to Symbol")

extract_sym(x) = error("$x $(typeof(x)) cant be converted to Symbol")
extract_sym(x::Int) = x
extract_sym(x::Symbol) = x
extract_sym(x::QuoteNode) = x.value

get_types(x::AbstractVector) = map(slot_name_t -> slot_name_t[2][2], x)

make_fieldname(x::Symbol) = string(x)
make_fieldname(x::Int) = "f$x"
