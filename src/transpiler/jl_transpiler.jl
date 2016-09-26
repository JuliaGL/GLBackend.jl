include("transpiler.jl")

type Backend{Identifier} <: TranspilerBackend
    #declarations::Dict{String, Declaration}
    current_slots::Vector{Dict{Any, Tuple{String, DataType}}}
    indentation_level::Int
    decl_io::IO
end
const jl_backend = Backend{:JL}(Dict{Slot, Tuple{String, DataType}}[], 1)

"""
Transpiled Expression
"""
immutable TExpr
    source::String
    typ::DataType
end
returntype(x::TExpr) = x.typ
source(x::TExpr) = x.source

typealias JL Backend{:JL}

"""
Type parameter are not supported, so we'll put them in the name
"""
function to_type_name(T)
    string(T.name)
end
function to_type_name(backend::JL, T::DataType)
    to_type_name(T)
end
function to_type_assert(backend::JL, name::String, typ::DataType)
    tn = to_type_name(backend, typ)
    to_type_assert(backend, name, tn)
end
function to_type_assert(backend::JL, name::String, typ::String)
    print(backend, "$typ::$name")
end

function declare_block(backend::JL, source::String, level=backend.indentation_level)
    repeat("    ", level)*source*";\n"
end


function declare_function(backend::JL, func::FuncExpr)
    types = get_types(func.args)
    name, rettype = get!(backend.function_map, (string(func.name), types)) do
        arg_decl = map(arg->to_type_assert(backend, arg[2]...), func.args)
        rettype = to_type_name(backend, func.returntype)
        transpilat = transpile(backend, func.body)
        src = source(transpilat)
        arg_end = last(func.args.indexes[1])
        var_decl = declare_slots(backend::JL, func.slots[(arg_end+1):end])
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

function transpile(backend::JL, f::Symbol, types)
    func = getfield(current_module(), f)
    transpile(backend, func, types)
end
function transpile(backend::JL, f::GlobalRef, types)
    func = getfield(f.mod, f.name)
    transpile(backend, func, types)
end
function transpile(backend::JL, f::Union{Function, Type}, types::AbstractVector)
    transpile(backend, f, tuple(types...))
end


function transpile(backend::JL, f::Union{Function, Type}, types::Tuple)
    name, rettype = get!(backend.function_map, (string(Symbol(f)), [types...])) do
        func = FuncExpr(f, types)
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

function transpile(backend::JL, expr::LineNumberNode)
    TExpr("#julia source line: $(expr.line)", Void)
end

"""
concrete values
"""
function transpile{T<:Number}(backend::JL, value::T)
    TExpr(string(value), T)
end

function transpile(backend::JL, T::Type)
    TExpr("Type{$T}", Type{T})
end

function transpile(backend::JL, expr::Union{Slot, SSAValue})
    src_t = last(backend.current_slots)[expr]
    TExpr(src_t...)
end

function transpile(backend::JL, node::NewvarNode)
    slot = node.slot
    var_decl = last(backend.current_slots)[slot]
    TExpr(var_decl...)
end


function transpile(backend::JL, expr::Expr, ::Val{:call})
    fun = shift!(expr.args)
    if isa(fun, Core.IntrinsicFunction)

    end
    if isbox(fun) # ignore boxes, LOL \( ﾟ◡ﾟ)/
        @assert length(expr.args) == 2
        ret_type, inner_expr = expr.args
        t = transpile(backend, inner_expr)
        @assert returntype(t) == ret_type
        return t
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
function transpile(backend::JL, expr::Expr, ::Val{:continue})
    TExpr("continue", Void)
end
function transpile(backend::JL, expr::Expr, ::Val{:break})
    TExpr("break", Void)
end
function transpile(backend::JL, expr::Expr, ::Val{:return})
    src = transpile(backend, expr.args[1])
    TExpr("return $(source(src))", returntype(src))
end
function transpile(backend::JL, expr::Expr, ::Val{:if})
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
function transpile(backend::JL, expr::Expr, ::Val{:while})
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
function transpile(backend::JL, expr::Expr, ::Val{:new})
    #push!(type_expr, expr)
    applytype = expr.args[1]
    T = _apply_type(applytype)
    args_t = map(x->transpile(backend, x), expr.args[2:end])
    transpilat = map(source, args_t)
    tname = to_type_name(T)
    TExpr("$(tname)($(join(transpilat, ", ")))", T)
end


function print_expr(backend::JL, x::Void, io=STDOUT, intend=1)
    # it's nothing, dude!
end
function print_expr(backend::JL, x::LineNumberNode, io=STDOUT, intend=1)

end
function print_expr(backend::JL, x::SSAValue, io=STDOUT, intend=1)

end
function print_expr(backend::JL, x::Slot, io=STDOUT, intend=1)

end
function print_expr(backend::JL, x::NewvarNode, io=STDOUT, intend=1)

end
function print_expr(backend::JL, x::Vector, io=STDOUT, intend=1)
    for elem in x
        print_expr(backend, elem, io, intend)
    end
end
function print_expr(backend::JL, x::Expr, io=STDOUT, intend=1)
    if x.head == :block
        print_expr(backend, x.args, io, intend)
    elseif x.head == :call


    end
end
