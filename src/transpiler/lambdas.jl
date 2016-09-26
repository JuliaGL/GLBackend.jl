immutable FuncExpr
    name::Symbol
    slots
    args
    body::Vector
    returntype::Type
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

function slot_mapping(lam_typed)
    if isa(lam_typed.slotnames, Void) || isa(lam_typed.slottypes, Void)
        return [(SlotNumber(-1), ("", Void))]
    end
    slotnames = copy(lam_typed.slotnames)
    slottypes = copy(lam_typed.slottypes)

    slots = [(SlotNumber(i), (string(name), slottypes[i])) for (i, name) in enumerate(slotnames)]
    ssaslot = [(SSAValue(i-1), ("ssa_$(i-1)", t)) for (i,t) in enumerate(lam_typed.ssavaluetypes)]
    vcat(slots, ssaslot)
end

function FuncExpr(f, types)
    if isa(f, Core.IntrinsicFunction)
        error("$f is an intrinsic function which can't be transpiled")
    end
    lam_typed, ret_type = get_lambda(code_typed, f, types)
    slots = slot_mapping(lam_typed)
    ast = lam_typed.code
    name = Symbol(f)
    FuncExpr(name, slots, view(slots, 2:(length(slots))), ast, ret_type)
end
