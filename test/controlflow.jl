include(Pkg.dir("GLBackend", "src", "transpiler", "gl_transpiler.jl"))

function flow2{T}(iter::T)
    z = T(0)
    if true
    end
    for i=T(1):iter
        if i == T(10)
            return T(i)
        elseif i==T(10)
            continue
        elseif i==T(10)
            break
        else
            return T(7)
        end
        if !(z == i)
            i = i+T(10)
        end
    end
    T(iter)
end
flow(10)


transpile(gl_backend, flow2, (Int,))
func = FuncExpr(flow, (Int32,))
map!(func.slots) do slot_name_t
    s, (n, t) = slot_name_t
    if applicable(GLAbstraction.gl_promote, t)
        t = GLAbstraction.gl_promote(t)
    end
    s, (n, t)
end
remove_static_param!(func.body, func.static_parameters)
x = remove_goto(func.body)











lam.specTypes
