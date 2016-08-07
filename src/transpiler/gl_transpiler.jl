
function insert_inbuild_types!(backend)
    types = (Float32, Float64, Int32, UInt32)
    typemap = Dict(
        Float32 => "float",
        Float64 => "double",
        Int32 => "int",
        UInt32 => "uint"
    )
    for i=1:4
        for T in (Vec, Point)
            for ET in types
                VT = T{i, ET}
                if i==1
                    typemap[VT] = typemap[ET]
                else
                    typemap[VT] = string(opengl_prefix(ET), "vec", i)
                end
                for j=1:4
                    typemap[Mat{i,j,ET}] = string(
                        opengl_prefix(ET), "mat", i==j ? "$i" : "$(i)x$(j)"
                    )
                end
            end
        end
    end
    typemap[RGB{Float32}] = "vec3"
    typemap[RGBA{Float32}] = "vec4"
    merge!(backend.typemap, typemap)
    nothing
end
function insert_inbuild_functions!(backend)
    buildins = Dict(
        2 => Symbol[:dot,:floor,:max,:any,:ldexp,:ceil,:normalize,:all,:log,:mod,:cross,:trunc,:round,:min]
        3 => Symbol[:round,:ceil,:trunc,:floor,:fma,:any,:all,:clamp]
        1 => Symbol[:atanh,:tanh,:exp2,:floor,:max,:any,:sin,:tan,:modf,:step,:log2,:acosh,:ceil,:normalize,:length,:atan,:all,:log,:isnan,:trunc,:asinh,:transpose,:abs,:frexp,:sqrt,:round,:sinh,:sign,:acos,:asin,:cosh,:exp,:min,:isinf,:cos]
    )
    # TODO actually figure out the signature of the GLSL build-in functions
    for i=1:3, ET in (Float32, Float64, Int32, UInt32), name in buildins[i]
        fs = FunctionSignature(name, [T for _=1:i], T)
        backend.declared_functions[fs] = name
    end
end

to_type_name(backend, T) = T.name.name
function to_type_assert(backend, name, typ)
    "$(to_type_name(backend, typ)) $(name)"
end

function define_composite_type{T}(backend, ::Type{T})
    fields = [to_type_assert(backend, n, fieldtype(T, n)) for n in fieldnames(T)]
    name = to_type_name(T)
    """
    struct $(name){
        $(join(fields, ";\n"));
    };
    """
end
function define_function(backend, name, args, body, returntype)
    args = map(arg->to_type_assert(backend, arg), args)
    """
    $(to_type_name(returntype)) $name($(args...)){
        $(transpile(backend, body))
    }
    """
end



function transpile_expr(backend, expr::Expr)

end

function insert_function(backend, name, types)

end
function calltype_lookup(backend, x::Expr, slot_dict)
    if x.head == :call
        f = expr.args[1]
        types = map(x->calltype_lookup(backend, x, slot_dict) ,expr.args[2:end])
        declare_function(backend, f, types)
        return returntype(backend, f, types)
    else
        error("No complex args allowed. Found: $x")
    end
end
function calltype_lookup(backend, x::SlotNumber, slot_dict)
    slot_dict[x][2]
end
function transpile(backend, block::Expr)
    if block.head == :block || block.head == :body
        join(map(block.args) do elem
            "$(transpile(backend, elem))"
        end, "\n")
    elseif expr.head == :call
        f = expr.args[1]
        types = map(x-> ,expr.args[2:end])
        declare_function(backend, f, types)
        sprint() do io
            print(io, expr, ";")
        end
    elseif expr.head == :(=)
        slot, expr = expr.args
        "$slot = $(transpile(backend, expr));"
    end
end
