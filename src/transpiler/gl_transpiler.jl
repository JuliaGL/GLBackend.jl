include("transpiler.jl")
using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
import GLAbstraction: opengl_prefix

@noinline function glsl_call{T}(glsl_str, ret_t::Type{T}, arg_t, args...)
    unsafe_load(Ptr{T}(C_NULL))::T
end

immutable Declaration
    dependencies::Vector{Declaration}
    source::String
end

type Backend{Identifier} <: TranspilerBackend
    #declarations::Dict{String, Declaration}
    current_slots::Vector{Dict{Any, Tuple{String, DataType}}}
    type_map::Dict{DataType, String}
    function_map::Dict{Tuple{String, Vector{DataType}}, Tuple{String, DataType}}
    indentation_level::Int
end

"""
Transpiled Expression
"""
immutable TExpr
    source::String
    typ::DataType
end
returntype(x::TExpr) = x.typ
source(x::TExpr) = x.source

typealias GL Backend{:GL}

function Backend(sym::Symbol)
    Backend{sym}(
        Dict{Slot, Tuple{String, DataType}}[],
        Dict{DataType, String}(),
        Dict{Tuple{String, Tuple}, Tuple{String, DataType}}(),
        1
    )
end

function insert_function!(backend::GL, jlfun, glsl_fun, types_ret::Pair)
    types, returntype = types_ret
    backend.function_map[(string(jlfun), types)] = (string(glsl_fun), returntype)
end

function insert_buildins!(backend::GL)
    # TODO do this in a cool way!
    # e.g define Module buildins with glsl inline and other tricks
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
    # TODO, don't just ignore Int64
    type_map[Int64] = "int"
    type_map[Bool] = "bool"
    type_map[RGBA{Float32}] = "vec4"
    merge!(backend.type_map, type_map)
    buildins = Dict(
        3 => Symbol[:round,:ceil,:trunc,:floor,:fma,:any,:all,:clamp],
        2 => Symbol[:atan, :floor,:max,:any,:ldexp,:ceil,:normalize,:all,:log,:mod,:cross,:trunc,:round,:min],
        1 => Symbol[:atanh,:tanh,:exp2,:floor,:max,:any,:sin,:tan,:modf,:step,:log2,:acosh,:ceil,:normalize,:length,:atan,:all,:log,:isnan,:trunc,:asinh,:transpose,:abs,:frexp,:sqrt,:round,:sinh,:sign,:acos,:asin,:cosh,:exp,:min,:isinf,:cos]
    )
    ops = (
        ".*" => "*",
        ".+" => "+",
        "./" => "/",
        "*" => "*",
        "+" => "+",
        "-" => "-",
        "/" => "/",
        # "div_float" => "/",
        # "sub_float" => "-",
        # "add_float" => "+"
    )
    boolfuns = [:all, :any]
    # TODO actually figure out the signature of the GLSL build-in functions
    glsl_types = unique(collect(keys(backend.type_map)))
    for T in glsl_types
        if !isa(T, Texture) && !isa(T, Mat)
            for i=1:3
                for fun in buildins[i]
                    insert_function!(backend, string(fun), string(fun), [T for _=1:i] => T)
                end
            end
            if T <: FixedVector
                insert_function!(backend, "dot", "dot", [T, T] => eltype(T))
            end
            for (jl_op, glsl_op) in ops
                insert_function!(backend, jl_op, glsl_op, [T, T] => T)
            end
            for jl_op in ("*", "-", "+")
                insert_function!(backend, jl_op, jl_op, [T] => T)
            end
        end
    end
    # cases not yet covered generically
    insert_function!(backend, "Float32", "float", [Float32] => Float32)
    insert_function!(backend, "Float32", "float", [Int] => Float32)
    insert_function!(backend, "Float32", "float", [Int32] => Float32)
    insert_function!(backend, "Float32", "float", [UInt] => Float32)
    insert_function!(backend, "Float32", "float", [UInt32] => Float32)
    insert_function!(backend, "Int32", "int", [UInt32] => Int32)
    insert_function!(backend, "Int32", "int", [Int32] => Int32)
    insert_function!(backend, "Int32", "int", [Float32] => Int32)
    insert_function!(backend, "Int32", "int", [Int] => Int32)

    insert_function!(backend, "Int64", "int", [UInt32] => Int64)
    insert_function!(backend, "Int64", "int", [Int32] => Int64)
    insert_function!(backend, "Int64", "int", [Float32] => Int64)
    insert_function!(backend, "Int64", "int", [Int] => Int64)

    insert_function!(backend, "norm", "normalize", [Vec3f0] => Float32)
    insert_function!(backend, "^", "pow", [Float32, Float32] => Float32)
    insert_function!(backend, "^", "pow", [Int32, Int32] => Float32)

    insert_function!(backend, Symbol(Vec3f0), "vec3", [Float32, Float32, Float32] => Vec3f0)
    insert_function!(backend, Symbol(Vec3f0), "vec3", [Vec3f0] => Vec3f0)
    bin_bool_ops = ("<", ">", "<=", ">=", "==", "&&", "||", "!=")
    types = (Float32, Float64, Int64, Int32)
    for T in types
        for op in bin_bool_ops
            insert_function!(backend, op, op, [T, T] => Bool)
        end
    end
    insert_function!(backend, "!", "!", [Bool] => Bool)
    insert_function!(backend, ".*", "*", [Vec3f0, Float32] => Vec3f0)
    insert_function!(backend, ".*", "*", [Float32, Vec3f0] => Vec3f0)
    insert_function!(backend, "*", "*", [Vec3f0, Float32] => Vec3f0)
    insert_function!(backend, "*", "*", [Float32, Vec3f0] => Vec3f0)
    insert_function!(backend, "*", "*", [Float32, Float32, Float32] => Float32)
    insert_function!(backend, "*", "*", [Float32, Float32] => Float32)

    insert_function!(backend, "+", "+", [Vec3f0, Vec3f0, Vec3f0] => Vec3f0)
    insert_function!(backend, "+", "+", [Float32, Float32, Float32] => Float32)
    insert_function!(backend, "+", "+", [Float32, Float32] => Float32)

end
Base.atan(a,b) = atan2(a,b)
"""
`typeof` is a generic function so we use this one instead
"""
function glsl_typeof{T}(x::T)
    T
end

"""
`glsl_ifelse` is not really supported in glsl
"""
function glsl_ifelse(condition, a, b)
    if condition
        return a
    else
        return b
    end
end


const gl_backend = Backend(:GL)
insert_buildins!(gl_backend)

isbox(x) = (x == GlobalRef(Base, :box))
extract_sym(T, x) = error("$x $(typeof(x)) cant be converted to Symbol")
extract_sym{T}(::Type{T}, x::Int) = make_fieldname(fieldname(T, x))
extract_sym(T, x::Symbol) = x
extract_sym(T, x::QuoteNode) = x.value
extract_sym(T, x) = error("$x $(typeof(x)) cant be converted to Symbol")

extract_sym(x) = error("$x $(typeof(x)) cant be converted to Symbol")
extract_sym(x::Int) = x
extract_sym(x::Symbol) = x
extract_sym(x::QuoteNode) = x.value

get_types(x::AbstractVector) = map(slot_name_t -> slot_name_t[2][2], x)

"""
Type parameter are not supported, so we'll put them in the name
"""
function to_type_name(T)
    join((T.name.name, T.parameters...), "_")
end
function to_type_name(backend::GL, T::DataType)
    get!(backend.type_map, T) do
        declare_composite_type(backend, T)
    end
end

function to_type_assert(backend::GL, name::String, typ::String)
    "$typ $name"
end
function to_type_assert(backend::GL, name::String, typ::DataType)
    tn = to_type_name(backend, typ)
    to_type_assert(backend, name, tn)
end

function declare_slots(backend::GL, slots)
    join(map(slots) do slot_name_t
        slot, (name, t) = slot_name_t
        slot_declare = to_type_assert(backend, name, t)
        declare_block(backend, slot_declare)
    end, "")
end

function declare_block(backend::GL, source::String, level=backend.indentation_level)
    repeat("    ", level)*source*";\n"
end


function declare_struct(backend, name, fnames, fieldtypes)
    fields = [to_type_assert(backend, make_fieldname(n), t) for (n, t) in zip(fnames, fieldtypes)]
    #TODO actualy insert this into declarations
    println("""
struct $(name){
$(join(fields, ";\n"));
};
    """)
    name
end
function declare_composite_type(backend::GL, T::DataType)
    name = to_type_name(T)
    get!(backend.type_map, T) do
        fnames = [n for n in 1:nfields(T)]
        ftypes = map(n->fieldtype(T, n), fnames)
        declare_struct(backend, name, fnames, ftypes)
        name
    end
end

function declare_function(backend::GL, func::FuncExpr)
    types = get_types(func.args)
    name, rettype = get!(backend.function_map, (string(func.name), types)) do
        arg_decl = map(arg->to_type_assert(backend, arg[2]...), func.args)
        rettype = to_type_name(backend, func.returntype)
        transpilat = transpile(backend, func.body)
        src = source(transpilat)
        arg_end = last(func.args.indexes[1])
        var_decl = declare_slots(backend::GL, func.slots[(arg_end+1):end])
        #@assert returntype(transpilat) == func.returntype
        name = func.name
        #TODO actualy insert this into declarations
        println("""
$rettype $name($(join(arg_decl, ", "))){
$var_decl
$src
}
""")
        string(name), func.returntype
    end
    name, rettype
end

function transpile(backend::GL, f::Symbol, types)
    func = getfield(current_module(), f)
    transpile(backend, func, types)
end
function transpile(backend::GL, f::GlobalRef, types)
    func = getfield(f.mod, f.name)
    transpile(backend, func, types)
end
function transpile(backend::GL, f::Union{Function, Type}, types::AbstractVector)
    transpile(backend, f, tuple(types...))
end


function transpile(backend::GL, f::Union{Function, Type}, types::Tuple)
    name, rettype = get!(backend.function_map, (string(Symbol(f)), [types...])) do
        func = FuncExpr(f, types)
        # convert slot types to glsl slot types
        # map!(func.slots) do slot_name_t
        #     s, (n, t) = slot_name_t
        #     if applicable(GLAbstraction.gl_promote, t)
        #         t = GLAbstraction.gl_promote(t)
        #     end
        #     s, (n, t)
        # end
        #remove_static_param!(func.body, func.static_parameters)
        nogotoexpr = remove_goto(func.body).args
        empty!(func.body)
        append!(func.body, nogotoexpr)
        push!(backend.current_slots, Dict(func.slots))
        decl = declare_function(backend, func)
        pop!(backend.current_slots)
        decl
    end
    name, rettype
end



function transpile(backend::GL, expr::LineNumberNode)
    TExpr("//julia source line: $(expr.line)", Void)
end

"""
concrete values
"""
function transpile(backend::GL, value::Number)
    #cnv_value = gl_convert(value)
    TExpr(string(value), typeof(value))
end

function transpile(backend::GL, T::Type)
    TExpr("Type{$T}", Type{T})
end

function transpile(backend::GL, expr::Union{Slot, SSAValue})
    src_t = last(backend.current_slots)[expr]
    TExpr(src_t...)
end
function transpile(backend::GL, expr::Expr, ::Val{:(=)})
    lh, rh = map(x->transpile(backend, x), expr.args)
    TExpr("$(source(lh)) = $(source(rh))", Void)
end
function transpile(backend::GL, node::NewvarNode)
    slot = node.slot
    var_decl = last(backend.current_slots)[slot]
    TExpr(var_decl...)
end


function transpile(backend::GL, expr::Expr, ::Val{:call})
    fun = shift!(expr.args)
    @show fun expr.args
    if isbox(fun) # ignore boxes, LOL \( ﾟ◡ﾟ)/
        @assert length(expr.args) == 2
        ret_type, inner_expr = expr.args
        t = transpile(backend, inner_expr)
        @assert returntype(t) == ret_type
        return t
    end
    if fun == GlobalRef(Core, :getfield)
        @assert length(expr.args) == 2
        expr, field = expr.args
        expr_trp = transpile(backend, expr)
        typ = returntype(expr_trp)
        field_s = extract_sym(typ, field)
        return TExpr("$(source(expr_trp)).$(field_s)", fieldtype(typ, extract_sym(field)))
    end
    if isa(fun, Expr)
        if fun.args[1] == GlobalRef(Core, :apply_type)
            tref = fun.args[2]
            T = tref.mod.(tref.name)
            fun = T{fun.args[3:end]...}
        end
    end
    if fun == GlobalRef(Base, :ifelse)
        fun = :glsl_ifelse
    end
    if fun == GlobalRef(Base, :typeof)
        fun = :glsl_typeof
    end

    args = map(x->transpile(backend, x), expr.args)
    argtypes = returntype.(args)
    transpilat = source.(args)
    if fun == GlobalRef(Core, :tuple)
        tup = Tuple{argtypes...}
        tname = to_type_name(tup)
        src = "$tname($(join(transpilat, ", ")))"
        return TExpr(src, tup)
    end
    fun_name, return_type = transpile(backend, fun, returntype.(args))
    call_expr = if Base.isoperator(Symbol(fun_name)) # most Julia operators should also be operators in GLSL
        if length(transpilat) == 1
            "$(fun_name)($(transpilat[1]))"
        else
            foldl(transpilat) do v0, v1
                "($v0 $fun_name $v1)"
            end
        end
    else
        "$fun_name($(join(transpilat, ", ")))"
    end
    TExpr(call_expr, return_type)
end
function transpile(backend::GL, expr::Expr, ::Val{:continue})
    TExpr("continue", Void)
end
function transpile(backend::GL, expr::Expr, ::Val{:break})
    TExpr("break", Void)
end
function transpile(backend::GL, expr::Expr, ::Val{:return})
    src = transpile(backend, expr.args[1])
    TExpr("return $(source(src))", returntype(src))
end
function transpile(backend::GL, expr::Expr, ::Val{:if})
    indspace = "   "^backend.indentation_level
    condition = transpile(backend, expr.args[1])
    #@assert returntype(condition) <: Bool "$condition"
    ifbody = source(transpile(backend, expr.args[2]))
    block_source = """$(indspace)if($(source(condition))){
$ifbody
$(indspace)}"""
    if length(expr.args) == 3
        elsebody = source(transpile(backend, expr.args[3]))
        block_source *= """else{
$elsebody
$(indspace)}"""
    end
    TExpr(block_source, Void)
end
function transpile(backend::GL, expr::Expr, ::Val{:while})
    indspace = "   "^backend.indentation_level
    condition = transpile(backend, expr.args[1])
    #@assert returntype(condition) <: Bool  "$condition"
    whilebody = source(transpile(backend, expr.args[2]))
    glslsource = """$(indspace)while($(source(condition))){
$whilebody
$(indspace)}"""
    TExpr(glslsource, Void)
end

function _apply_type(expr)
    @assert expr.head == :call
    @assert expr.args[1] == GlobalRef(Core, :apply_type)
    tref = expr.args[2]
    T = tref.mod.(tref.name)
    typ = T{expr.args[3:end]...}
end
function transpile(backend::GL, expr::Expr, ::Val{:new})
    #push!(type_expr, expr)
    applytype = expr.args[1]
    T = _apply_type(applytype)
    args_t = map(x->transpile(backend, x), expr.args[2:end])
    transpilat = map(source, args_t)
    tname = to_type_name(T)
    TExpr("$(tname)($(join(transpilat, ", ")))", T)
end
