using FixedSizeArrays, GeometryTypes, Colors, GLAbstraction
include(Pkg.dir("GLBackend", "src", "transpiler", "gl_transpiler.jl"))

blockidx() = 1
blockdim() = 1
threadidx() = 1

immutable GLArray{T, N} <: AbstractArray
end
Base.linearindexing{A<:GLArray}(::Type{A}) = Base.LinearFast()
Base.size{T, N}(g::GLArray{T, N}) = Vec{N, Int}(20)
function Base.getindex{N,T}(p::GLArray{N,T}, ind::Int)
    unsafe_load(Ptr{T}(C_NULL))::T
end
function Base.setindex!{N, T}(p::GLArray{N, T}, val::T, ind::Int)
    p
end
function broadcast_kernel2{T}(A, f, arg_1, arg_2::T)
    i = (blockidx()-1) * blockdim() + threadidx()
    if i < length(A) && i > 0
        idx = CartesianIndex(ind2sub(size(A), Int(i)))
        A[idx] = f(arg_1[idx], T(arg_2))
    end
    nothing
end
transpile(gl_backend, broadcast_kernel2, (GLArray{Float32, 2}, typeof(*), GLArray{Float32, 2}, Float32))
