x = readlines("inbuild_funs.txt");
xmls = filter(e->endswith(e, ".xml\n"), x)
# TODO actually parse XML
map!(xmls) do x
    replace(x, ".xml\n", "")
end
final = filter(xmls) do x
    isdefined(Symbol(x)) # only usefull if also defined in julia
end
n_ary_map = Dict{Int, Set{Symbol}}()
for elem in final
    f = Symbol(elem)
    for m in methods(eval(f))
        i = length(m.sig.parameters) - 1
        set = get!(n_ary_map, i, Set{Symbol}())
        push!(set, f)
    end
end

open("gl_inbuild_funs.jl") do io
    println(io, "Dict(")
    for (k, v) in n_ary_map
        println(io, "    ", k, " => ", collect(v))
    end
    println(io, ")")
end
