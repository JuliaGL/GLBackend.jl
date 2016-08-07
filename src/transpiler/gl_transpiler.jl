using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
import GLAbstraction: opengl_prefix

immutable Backend{Identifier} <: TranspilerBackend
    type_map::Dict{DataType, String} #
    function_map::Dict{String, Dict{Pair, String}}
end

"""
Transpiled Expression
"""
immutable TExpr
    source::String
    typ::String
end
ret_type(x::TExpr) = x.typ
source(x::TExpr) = x.source

immutable Declaration
    dependencies::Vector{Declaration}
    source::String
end


typealias GL Backend{:GL}

function Backend(sym::Symbol)
    Backend{sym}(
        Dict{DataType, String}(),
        Dict{String, Dict{Pair, String}}()
    )
end

function insert_function!(backend::GL, jlfun::String, glsl_fun::String, types::Pair)
    sigs = get!(backend.function_map, jlfun, Dict{Pair, String}())
    sigs[types] = glsl_fun
end

function insert_buildins!(backend::GL)
    types = (Float32, Float64, Int32, UInt32)
    type_map = Dict(
        Float32 => "float",
        Float64 => "double",
        Int32   => "int",
        UInt32  => "uint"
    )
    for i=1:4
        for T in (Vec, Point)
            for ET in types
                VT = T{i, ET}
                if i==1
                    type_map[VT] = type_map[ET]
                else
                    type_map[VT] = string(opengl_prefix(ET), "vec", i)
                end
                for j=1:4
                    type_map[Mat{i,j,ET}] = string(
                        opengl_prefix(ET), "mat", i==j ? "$i" : "$(i)x$(j)"
                    )
                end
            end
        end
    end
    type_map[RGB{Float32}] = "vec3"
    type_map[RGBA{Float32}] = "vec4"
    merge!(backend.type_map, type_map)
    buildins = Dict(
        3 => Symbol[:round,:ceil,:trunc,:floor,:fma,:any,:all,:clamp],
        2 => Symbol[:dot,:floor,:max,:any,:ldexp,:ceil,:normalize,:all,:log,:mod,:cross,:trunc,:round,:min],
        1 => Symbol[:atanh,:tanh,:exp2,:floor,:max,:any,:sin,:tan,:modf,:step,:log2,:acosh,:ceil,:normalize,:length,:atan,:all,:log,:isnan,:trunc,:asinh,:transpose,:abs,:frexp,:sqrt,:round,:sinh,:sign,:acos,:asin,:cosh,:exp,:min,:isinf,:cos]
    )
    ops = (
        ".*" => "*",
        ".+" => "+",
        "./" => "/",
        "*" => "*",
        "+" => "+",
        "/" => "/",
    )
    boolfuns = [:all, :any]
    # TODO actually figure out the signature of the GLSL build-in functions
    glsl_types = unique(collect(values(backend.type_map)))
    for T in glsl_types
        if !contains(T, "sampler") && !contains(T, "mat")
            for i=1:3
                for fun in buildins[i]
                    insert_function!(backend, string(fun), string(fun), [T for _=1:i] => T)
                end
            end
            for (jl_op, glsl_op) in ops
                insert_function!(backend, jl_op, glsl_op, (T, T) => T)
            end
        end
    end
end

const GLBackend = Backend(:GL)
insert_buildins!(GLBackend)

to_type_name(backend::GL, T) = backend.type_map[T]

function to_type_assert(backend::GL, name::String, typ::String)
    "$typ $name"
end
function to_type_assert(backend::GL, name, typ)
    "$(to_type_name(backend, typ)) $(name)"
end

function declare_composite_type{T}(backend::GL, ::Type{T})
    fields = [to_type_assert(backend, n, fieldtype(T, n)) for n in fieldnames(T)]
    name = to_type_name(T)
    """
    struct $(name){
        $(join(fields, ";\n"));
    };
    """
end
function declare_function(backend::GL, name, args, body, returntype)
    args = map(arg->to_type_assert(backend, arg), args)
    """
    $(to_type_name(returntype)) $name($(args...)){
        $(transpile(backend, body))
    }
    """
end
function declare_block(backend::GL, source::String)
    source*";\n"
end
function transpile(backend::GL, expr::Expr, ::Val{:(=)})
    lh, rh = map(x->transpile(backend, x), expr.args)
    @show typeof(lh)
    TExpr("$lh = $rh", Void)
end
function transpile(backend::GL, expr::Expr, ::Val{:call})
    fun = expr.args[1]
    if fun == GlobalRef(Base, :box) # ignore boxes, LOL
        returntype = expr.args[2]
        t = transpile(backend, expr[3], ::Val{:call})
        @assert ret_type(t) == returntype
        return TExpr(t.code,  ret_type(t))
    end
    args = map(x->transpile(backend, x), expr.args[2:end])
    return_type = declare_function(backend, f, args.ret_type.(args))
    call_expr = if Base.isoperator(fun)
        @assert length(args) == 2
        "$(args[1]) $fun $(args[2])"
    else
        "$fun($(join(args, ", ")))"
    end
    TExpr(call_expr, return_type)
end
function transpile(backend::GL, expr::Expr, ::Val{:line})
    expr = transpile(backend, expr)

end
function transpile(backend::GL, expr::Expr, ::Val{:if})
    cond, _if, _else = map(x->transpile(backend, x), expr.args)
    TExpr("""
    if($cond){
        $_if
    }else{
        $_else
    }
    """, Void)
end

function type_lookup(backend::GL, x::SlotNumber)
    backend.current_slots[x][2]
end
function type_lookup(backend::GL, x::TExpr)
    ret_type(x)
end
function type_lookup(backend::GL, x::Expr)
    if x.head == :(::)
        return x.args[2]
    end
    error("I'm afraid I cant infer type of $x")
end

function transpile(backend::GL, block::Expr)
    if block.head == :block || block.head == :body
        lines = map(block.args) do elem
            transpile(backend, elem)
        end
        block_source = join(lines) do line
            declare_block(backend, source(line))
        end
        return TExpr(join(block_source, ""), ret_type(last(lines)))
    else
        return transpile(backend, block, Val{block.head}())
    end
end
