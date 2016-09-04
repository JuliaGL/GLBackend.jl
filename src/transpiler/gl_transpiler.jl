using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
import GLAbstraction: opengl_prefix
abstract TranspilerBackend
immutable Backend{Identifier} <: TranspilerBackend
    declarations::Dict{String, Declaration}
    type_map::Dict{DataType, String}
    function_map::Dict{String, Dict{Pair, String}}
    current_slots::Dict{Slot, Tuple{String,DataType}}
end

"""
Transpiled Expression
"""
immutable TExpr
    source::String
    typ::String
end
returntype(x::TExpr) = x.typ
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
        # "div_float" => "/",
        # "sub_float" => "-",
        # "add_float" => "+"
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

function declare_block(backend::GL, source::String)
    source*";\n"
end
isbox(x) = (x == GlobalRef(Base, :box))


function to_type_name(backend::GL, T::DataType) 
    get!(backend.type_map, T) do
        declare_composite_type(backend, T)
    end
end
function to_type_name(T)
    join((T.name.name, T.parameters...), "_")
end
function to_type_assert(backend::GL, name::String, typ::String)

    "$typ $name"
end
function to_type_assert(backend::GL, name, typ)
    tn = to_type_name(backend, typ)
    to_type_assert(name, tn)
end

function declare_slots(backend::GL, slots)
    join(map(slots[2:end]) do slot_name_t
        slot, (name, t) = slot_name_t
        slot_declare = to_type_assert(backend, name, t)
        declare_block(backend, slot_declare)
    end, "")
end


function declare_composite_type{T}(backend::GL, ::Type{T})
    name = to_type_name(T)
    get!(backend.type_map, T) do
        fields = [to_type_assert(backend, n, fieldtype(T, n)) for n in fieldnames(T)]
        """
        struct $(name){
            $(join(fields, ";\n"));
        };
        """
    end
    name
end

get_types(x::Vector) = map(slot_name_t -> slot_name_t[2][2], x)

function declare_function(backend::GL, func::FuncExpr)
    types = get_types(func.args)
    name, rettype = get!(backend.function_map, (func.name, types))
        arg_decl = map(arg->to_type_assert(backend, arg[2]...), args)
        """
        $(to_type_name(returntype)) $name($(arg_decl...)){
            $(transpile(backend, body))
        }
        """
    end
    name, rettype
end


function transpile(backend::GL, expr::Slot)
    backend.current_slots[backend][1]
end
function transpile(backend::GL, expr::Expr, ::Val{:(=)})
    lh, rh = map(x->transpile(backend, x), expr.args)
    TExpr("$lh = $rh", Void)
end

function transpile(backend::GL, expr::Expr, ::Val{:call})
    fun = shift!(expr)
    if isbox(fun) # ignore boxes, LOL \( ﾟ◡ﾟ)/
        @assert length(expr.args) == 2
        returntype, inner_expr = expr.args
        t = transpile(backend, inner_expr)
        @assert returntype(t) == returntype
        return t
    end
    # transpile function arguments
    args = map(x->transpile(backend, x), expr.args[2:end])
    fun_name, return_type = transpile(backend, fun, returntype.(args))
    transpilat = source.(args)
    call_expr = if Base.isoperator(fun_name) # most Julia operators should also be operators in GLSL
        @assert length(args) == 2
        "$(transpilat[1]) $fun_name $(transpilat[2])"
    else
        "$fun_name($(join(transpilat, ", ")))"
    end
    TExpr(call_expr, return_type)
end

function transpile(backend::GL, expr::Expr, ::Val{:if})
    cond, _if, _else = map(x->transpile(backend, x), expr.args)
    TExpr("""
    if($cond){
        $_if
    }else{
        $_else
    }
    """, Void) # Semantic change between Julia vs GLSL. Retype in Julia would be smth like Union{_if, _else}
end

function type_lookup(backend::GL, x::SlotNumber)
    backend.current_slots[x][2]
end
function type_lookup(backend::GL, x::TExpr)
    returntype(x)
end
function type_lookup(backend::GL, x::Expr)
    if x.head == :(::)
        return x.args[2]
    end
    error("I'm afraid I cant infer type of $x, dave")
end

