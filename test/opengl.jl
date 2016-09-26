using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
include(Pkg.dir("GLBackend", "src", "transpiler", "gl_transpiler.jl"))

function mandelbulb{T}(x0::T,y0::T,z0::T, n, iter)
    x,y,z = x0,y0,z0
    for i=1:iter
        r = sqrt(x*x + y*y + z*z )
        theta = atan2(sqrt(x*x + y*y) , z)
        phi = atan2(y,x)
        rn = r^n
        x1 = rn * sin(theta*n) * cos(phi*n) + x0
        y1 = rn * sin(theta*n) * sin(phi*n) + y0
        z1 = rn * cos(theta*n) + z0
        (x1*x1 + y1*y1 + z1*z1) > n && return T(i)
        x,y,z = x1,y1,z1
    end
    T(iter)
end
n = 20
vol = zeros(Float32, n, n, n);
x = linspace(-1f0, 1f0, n);
x1 = reshape(x, (n, 1, 1));
x2 = reshape(x, (1, n, 1));
x3 = reshape(x, (1, 1, n));
vol .= mandelbulb.(x1, x2, x3, 8f0, 10)
f = mandelbulb; B = vol; As=(x1, x2, x3, 8f0, 10)
shape = indices(B)
Base.Broadcast.check_broadcast_shape(shape, As...)
keeps, Idefaults = Base.Broadcast.map_newindexer(shape, As)
keeps
Idefaults
lolcast!(f, B, keeps, Idefaults, As, Val{5})
using Base.Cartesian
@generated function lolcast!{K,ID,AT,nargs}(f, B::AbstractArray, keeps::K, Idefaults::ID, As::AT, ::Type{Val{nargs}})
    expr = quote
        $(Expr(:meta, :noinline))
        # destructure the keeps and As tuples
        @nexprs $nargs i->(A_i = As[i])
        @nexprs $nargs i->(keep_i = keeps[i])
        @nexprs $nargs i->(Idefault_i = Idefaults[i])
        @simd for I in CartesianRange(indices(B))
            # reverse-broadcast the indices
            @nexprs $nargs i->(I_i = Base.Broadcast.newindex(I, keep_i, Idefault_i))
            # extract array values
            @nexprs $nargs i->(@inbounds val_i = A_i[I_i])
            # call the function and store the result
            @inbounds B[I] = @ncall $nargs f val
        end
    end
    println(macroexpand(expr))
    expr
end



function test(vol, x)
    vol .= mandelbulb.(x, x, x, 8f0, 8)
end
@time test(vol, x);
function test2(vol, r)
    @inbounds for i=1:size(vol, 1), j=1:size(vol, 2), k=1:size(vol, 3)
        x,y,z = r[i], r[j], r[k]
        vol[i,j,k] = 8f0
    end
    vol
end
x = CartesianRange(indices(rand(10,10,10)))
function Base.getindex{N,T}(p::GLVisualize.Grid{N,T}, inds::CartesianIndex{N})
    Point{N, eltype(T)}(ntuple(Val{N}) do i
        p.dims[i][inds[i]]
    end)
end
using FixedSizeArrays
immutable Grid{N, T <: Range, ET} <: AbstractArray{ET, N}
    dims::NTuple{N, T}
end
function Grid{N, T<:Range}(x::Vararg{T, N})
    Grid{N, T, eltype(T)}(x)
end
Base.linearindexing{A<:Grid}(::Type{A}) = Base.LinearFast()
Base.size{N,T}(g::Grid{N,T}) = map(length, g.dims)
function Base.getindex{N,T}(p::Grid{N,T}, ind::Int)
    inds = ind2sub(size(p), ind)
    Vec{N, eltype(T)}(ntuple(Val{N}) do i
        p.dims[i][inds[i]]
    end)
end
n = 20
vol = zeros(Float32, n, n, n)
x = linspace(-1f0, 1f0, n)
g = Grid(x,x,x)
vol .= mandelbulb.(view(x, 1:n, 1:1, 1:1), view(x, 1:1, 1:n, 1:1), view(x, 1:1, 1:1, 1:n), 8f0, 8)
shape = indices(vol)
Base.Broadcast.check_broadcast_shape(shape, g)
keeps, Idefaults = Base.Broadcast.map_newindexer(shape, (1:5, 1:5, 1:5))
f, B, keeps, Idefaults, As, Val{nargs})
@which Base.Broadcast._broadcast!((a...)->a, vol, keeps, Idefaults, (1:5, 1:5, 1:5), Val{3})


fe = transpile(gl_backend, mandel3, (Float32, Float32, Float32, Float32, Int))
func = FuncExpr(mandel, (Float32, Float32, Float32, Float32, Int))
remove_static_param!(func.body, func.static_parameters)
for elem in func.body
    if !(isa(elem, Void) || isa(elem, LineNumberNode))
        println(elem)
    end
end
x = remove_goto(func.body)

function test(x, a, b)
    x .= sin.((+).(a,b))
end
@code_lowered test(zeros(10), rand(10), rand(10))
