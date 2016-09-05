module GLSLBuildins

# not in base, so inferable stump must be defined
gl_FragCoord() = zero(Vec2f0)
# replace rule for transpiler
gl_FragCoord(backend::GL) = TGlobal("gl_FragCoord", Vec2f0)

gl_InstanceID() = Int32(0)
gl_InstanceID(backend::GL) = TGlobal("gl_InstanceID", Vec2f0)


#Base.sin(x) already defined
sin{T}(backend::GL, x::Type{T}) = TExpr("sin", T)


EmitVertex() = nothing
sin{T}(backend::GL) = TExpr("EmitVertex", T)

end
