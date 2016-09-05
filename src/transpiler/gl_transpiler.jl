using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
import GLAbstraction: opengl_prefix
abstract TranspilerBackend

immutable FuncExpr
    name::Symbol
    slots
    args
    body::Vector
    returntype::DataType
    static_parameters::Tuple
end

immutable Light{T}
    position::Vec{3,T}
    ambient::Vec{3,T}
    diffuse::Vec{3,T}
    diffuse_power::T
    specular::Vec{3,T}
    specular_power::T
end

immutable Shading{T}
    ambient::Vec{3, T}
    specular::Vec{3, T}
    shininess::T
end
function get_lambda(pass, f, types)
    lambda = pass(f, types)
    if isa(lambda, Vector)
        if isempty(lambda)
            args = join(map(t->"::$t", types), ", ")
            error("$f($args) couldn't be found")
        end
        return first(lambda)
    else
        isa(lambda, LambdaInfo) && return lambda
        error("Not sure what's up with returntype of $pass. Returned: $lambda")
    end
end

function FuncExpr(f, types)
    lam_lowered = get_lambda(code_lowered, f, types)
    lam_typed = get_lambda(code_typed, f, types)
    slotnames = Base.lambdainfo_slotnames(lam_lowered)
    slottypes = lam_typed.slottypes
    nargs = length(types)
    slots = [(SlotNumber(i), (replace(name, "@", ""), slottypes[i])) for (i, name) in enumerate(slotnames)]
    ast = Base.uncompressed_ast(lam_lowered)
    name = Symbol(f)
    static_params = tuple(lam_typed.sparam_vals...)
    FuncExpr(name, slots, view(slots, 2:(nargs+1)), ast, lam_typed.rettype, static_params)
end


immutable Declaration
    dependencies::Vector{Declaration}
    source::String
end

type Backend{Identifier} <: TranspilerBackend
    #declarations::Dict{String, Declaration}
    current_slots::Vector{Dict{Slot, Tuple{String, DataType}}}
    type_map::Dict{DataType, String}
    function_map::Dict{Tuple{String, Vector{DataType}}, Tuple{String, DataType}}
    indentation_level::Int
end
type GTBlock
    entry
    block
    out
end
GTBlock(entry) = GTBlock(entry, [], Nullable{GotoNode}())
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
    type_map[RGBA{Float32}] = "vec4"
    merge!(backend.type_map, type_map)
    buildins = Dict(
        3 => Symbol[:round,:ceil,:trunc,:floor,:fma,:any,:all,:clamp],
        2 => Symbol[:floor,:max,:any,:ldexp,:ceil,:normalize,:all,:log,:mod,:cross,:trunc,:round,:min],
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

    insert_function!(backend, "norm", "normalize", [Vec3f0] => Float32)
    insert_function!(backend, "^", "pow", [Float32, Float32] => Float32)
    insert_function!(backend, "^", "pow", [Int32, Int32] => Float32)

    insert_function!(backend, Symbol(Vec3f0), "vec3", [Float32, Float32, Float32] => Vec3f0)
    insert_function!(backend, Symbol(Vec3f0), "vec3", [Vec3f0] => Vec3f0)
    insert_function!(backend, "<=", "<=", [Float32, Float32] => Bool)
    insert_function!(backend, ".*", "*", [Vec3f0, Float32] => Vec3f0)
    insert_function!(backend, ".*", "*", [Float32, Vec3f0] => Vec3f0)
    insert_function!(backend, "*", "*", [Vec3f0, Float32] => Vec3f0)
    insert_function!(backend, "*", "*", [Float32, Vec3f0] => Vec3f0)
    insert_function!(GLBackend, "+", "+", [Vec3f0, Vec3f0, Vec3f0] => Vec3f0)

end

const GLBackend = Backend(:GL)
insert_buildins!(GLBackend)

isbox(x) = (x == GlobalRef(Base, :box))
extract_sym(x) = error("$x $(typeof(x)) cant be converted to Symbol")
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
function declare_composite_type(backend::GL, T::DataType)
    name = to_type_name(T)
    get!(backend.type_map, T) do
        fields = [to_type_assert(backend, string(n), fieldtype(T, n)) for n in fieldnames(T)]
        #TODO actualy insert this into declarations
        println("""
struct $(name){
$(join(fields, ";\n"));
};
        """)
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

function remove_static_param!(expr, static_parameters)
    expr # leave any other untouched
end
function remove_static_param!(expr::Vector, static_parameters)
    map!(e->remove_static_param!(e, static_parameters), expr)
end
function remove_static_param!(expr::Expr, static_parameters)
    if expr.head == :static_parameter
        idx = expr.args[1]
        return static_parameters[idx]
    else
        remove_static_param!(expr.args, static_parameters)
        expr
    end
end
function transpile(backend::GL, f::Union{Function, Type}, types::Tuple)
    name, rettype = get!(backend.function_map, (string(Symbol(f)), [types...])) do
        func = FuncExpr(f, types)
        # convert slot types to glsl slot types
        map!(func.slots) do slot_name_t
            s, (n, t) = slot_name_t
            if applicable(GLAbstraction.gl_promote, t)
                t = GLAbstraction.gl_promote(t)
            end
            s, (n, t)
        end
        remove_static_param!(func.body, func.static_parameters)
        push!(backend.current_slots, Dict(func.slots))
        decl = declare_function(backend, func)
        pop!(backend.current_slots)
        decl
    end
    name, rettype
end

function transpile(backend::GL, block::Expr)
    if block.head == :block || block.head == :body
        transpile(backend, block.args)
    else
        # I knoow, Val no good here... But it makes the function overwritable and composable, which might be nice
        # in order to have unspecialized fall backs which can be specialized
        #println("will transpile: $(block.head) ", block)
        return transpile(backend, block, Val{block.head}())
    end
end


function is_if(block::GTBlock)
    (
        isa(block.entry, Expr) &&
        block.entry.head == :gotoifnot
    )
end
function is_elseif(block::GTBlock)
    (
        isa(block.entry, Tuple) &&
        isa(first(block.entry), LabelNode) &&
        isa(last(block.entry), Expr) &&
        last(block.entry).head == :gotoifnot
    )
end
function is_else(block::GTBlock)
    isa(block.entry, LabelNode) && isnull(block.out)
end

# (╯°□°）╯︵ ┻━┻
function deal_with_goto(backend, elem, state, list, block_source)
    labels = Set(Int[])
    blocks = GTBlock[]
    while true
        if isa(elem, Expr) && elem.head == :gotoifnot
            condition, label = elem.args
            push!(labels, label)
            push!(blocks, GTBlock(elem))
        elseif (isa(elem, LabelNode) && elem.label in labels)
            setdiff!(labels, elem.label)
            isempty(labels) && break # found last label
            elem0, state0 = elem, state
            elem, state = next(list, state)
            while isa(elem, LineNumberNode)
                elem, state = next(list, state)
            end
            if !(isa(elem, Expr) && elem.head == :gotoifnot)
                elem, state = elem0, state0 # reset peek
                push!(blocks, GTBlock(elem))
            else
                push!(labels, elem.args[2])
                push!(blocks, GTBlock((elem0, elem)))
            end
        elseif isa(elem, GotoNode)
            last(blocks).out = Nullable(elem) #close block
            push!(labels, elem.label)
        elseif isa(elem, Expr) && elem.head == :gotoifnot
            condition, label = elem.args
            push!(labels, label)
        else
            push!(last(blocks).block, elem) # insert statements of block
        end
        done(list, state) && break
        elem, state = next(list, state)
    end
    if done(list, state) && !isempty(labels)
        error("Oh my... Couldn't find all goto labels")
    end
    indents = backend.indentation_level
    indspace = repeat("    ", indents)
    backend.indentation_level += 1
    for block in blocks
        bodysrc = source(transpile(backend, block.block))
        if is_if(block)
            cond = block.entry.args[1]
            condsrc = source(transpile(backend, cond))
            block_source *= """
$(indspace)if($condsrc){
$bodysrc
$(indspace)}
"""
        elseif is_elseif(block)
            cond = block.entry[2].args[1]
            condsrc = source(transpile(backend, cond))
            block_source *= """
$(indspace)else if($condsrc){
$bodysrc
$(indspace)}
"""
        elseif is_else(block)
            block_source *= """
$(indspace)else{
$bodysrc
$(indspace)}
"""
        else
            error("Block $block not supported yet")
        end
    end
    backend.indentation_level = indents
    elem, state = next(list, state) # move one after last labelnode
    elem, state, block_source
end

function transpile(backend::GL, list::AbstractVector)
    last_type = Any
    block_source = ""
    state = start(list)
    while !done(list, state)
        elem, state = next(list, state)
        if isa(elem, Expr) && (elem.head == :gotoifnot || isa(elem, GotoNode))
            elem, state, block_source = deal_with_goto(backend, elem, state, list, block_source)
        end
        line = transpile(backend, elem)
        line == nothing && continue
        block_source *= declare_block(backend, source(line))
        last_type = returntype(line)
    end
    return TExpr(block_source, last_type)
end


transpile(backend::GL, expr::Void) = nothing

function transpile(backend::GL, expr::LineNumberNode)
    TExpr("//julia source line: $(expr.line)", Void)
end
"""
concrete values
"""
function transpile(backend::GL, value)
    cnv_value = gl_convert(value)
    TExpr(string(cnv_value), typeof(cnv_value))
end

function transpile(backend::GL, expr::Slot)
    src_t = last(backend.current_slots)[expr]
    TExpr(src_t...)
end
function transpile(backend::GL, expr::Expr, ::Val{:(=)})
    lh, rh = map(x->transpile(backend, x), expr.args)
    TExpr("$(source(lh)) = $(source(rh))", Void)
end


function transpile(backend::GL, expr::Expr, ::Val{:call})
    fun = shift!(expr.args)

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
        field_s = extract_sym(field)
        typ = returntype(expr_trp)
        return TExpr("$(source(expr_trp)).$(field_s)", fieldtype(typ, field_s))
    end
    args = map(x->transpile(backend, x), expr.args)

    fun_name, return_type = transpile(backend, fun, returntype.(args))
    transpilat = source.(args)
    call_expr = if Base.isoperator(Symbol(fun_name)) # most Julia operators should also be operators in GLSL
        if length(transpilat) == 1
            "$(fun_name)$(transpilat[1])"
        else
            foldl(transpilat) do v0, v1
                "$v0 $fun_name $v1"
            end
        end
    else
        "$fun_name($(join(transpilat, ", ")))"
    end
    TExpr(call_expr, return_type)
end
function transpile(backend::GL, expr::Expr, ::Val{:return})
    src = transpile(backend, expr.args[1])
    TExpr("return $(source(src))", returntype(src))
end


# Test

function blinnphong3{NV, T}(light, L::Vec{NV, T}, N, V, color, shading)
    diff_coeff = max(dot(L,N), T(0.0))
    # specular coefficient
    H = normalize(L+V)

    spec_coeff = max(dot(H,N), 0.0)^(shading.shininess)
    if diff_coeff <= 0.0
        spec_coeff = 0.0
    elseif diff_coeff <= 0.2
        # some nonesense to test elseif
        spec_coeff *= 2.0;
        spec_coeff += 1.0;
    else
        spec_coeff = spec_coeff;
    end
    # final lighting model
    return Vec3f0(
        light.ambient .* shading.ambient +
        light.diffuse .* light.diffuse_power .* color * diff_coeff +
        light.specular .* light.specular_power .* shading.specular * spec_coeff
    )
end


l = Light(Vec3f0(0), Vec3f0(0), Vec3f0(0), 1f0, Vec3f0(0), 1f0)
s = Shading(Vec3f0(0), Vec3f0(0), 1f0)
blinnphong3(l, Vec3f0(0), Vec3f0(0), Vec3f0(0), Vec3f0(0), s)
fe = FuncExpr(blinnphong3, (Light{Float32},Vec3f0, Vec3f0, Vec3f0, Vec3f0,Shading{Float32}))
fe.slots
transpile(GLBackend, blinnphong3, (Light{Float32},Vec3f0, Vec3f0, Vec3f0, Vec3f0,Shading{Float32}))
#delete!(GLBackend.function_map, ("blinnphong3", [Light{Float32},Vec3f0, Vec3f0, Vec3f0, Vec3f0,Shading{Float32}]))
