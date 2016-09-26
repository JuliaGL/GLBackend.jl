"""
Peeks `n`(can be positive or negative) steps from `pos` into list.
Returns nothing if out of bounds and jumps over nothing and LineNumberNode's
"""
function peek_n(n, list, pos)
    i = pos; step = sign(n)
    while abs(n) > 0
        (i<1 || i>length(list)) && return nothing
        elem = list[i]
        if !(isa(elem, LineNumberNode) || isa(elem, Void))
            n -= step
        end
        i += step
    end
    list[i]
end

"""
Determines if `elem` is a `unless`/`gotoifnot` node
"""
isa_unless(elem) = isa(elem, Expr) && elem.head == :gotoifnot

"""
Replaces `goto` statements in a loop body with continue and break.
"""
function replace_continue_break!(list, continue_label, break_label)
    map!(list) do elem
        if isa(elem, GotoNode) && elem.label == continue_label.label
            Expr(:continue)
        elseif isa(elem, GotoNode) && elem.label == break_label.label
            Expr(:break)
        else
            elem
        end
    end
end

"""
Removes all gotos, labels and nothing's inplace recursively in `Expr` `expr`.
"""
remove_labelgoto!(expr::Expr) = (_remove_labelgoto!(expr); nothing)
_remove_labelgoto!(x) = true
_remove_labelgoto!(x::Union{LabelNode, GotoNode, Void}) = false
function _remove_labelgoto!(x::Expr)
    filter!(_remove_labelgoto!, x.args)
    true
end

"""
Checks if nodes at `startpos` and `labelpos` define a loop body.
If so it will build up the matching loop expression and
`return true, LoopExpr, labelpos`.
Else it'll `return false, Expr(:block), labelpos`
"""
function handle_loop(list, startpos, labelpos, body)
    unless, breaklabel = list[startpos], list[labelpos]
    @assert unless.args[2] == breaklabel.label # we should have made this sure already
    looplabel = peek_n(-1, list, startpos)
    isa(looplabel, LabelNode) || return false, Expr(:block), labelpos
    goto = peek_n(-1, list, labelpos)
    isa(goto, GotoNode) || return false, Expr(:block), labelpos
    looplabel.label == goto.label || return false, Expr(:block), labelpos
    continuelabel = peek_n(-2, list, labelpos)
    isa(continuelabel, LabelNode) || return false, Expr(:block), labelpos
    # now that we have continue/break labels, we can replace the gotos
    replace_continue_break!(body.args, continuelabel, breaklabel)
    x = _remove_goto(body.args)
    true, Expr(:while, unless.args[1], x), labelpos+1
end

"""
Checks if nodes at `startpos` and `labelpos` define an if body.
If so it will build up the matching if(else) expression and
`return true, LoopExpr, labelpos`.
Else it'll `return false, Expr(:block), labelpos`
"""
function handle_if(list, startpos, labelpos, ifbody)
    unless = list[startpos]
    if_expr = Expr(:if, unless.args[1], _remove_goto(ifbody.args))
    goto = peek_n(-1, list, labelpos)
    if isa(goto, GotoNode) # that means we're dealing with else
        pos = labelpos; else_body = Expr(:block)
        for i=pos:length(list)
            elem = list[i]
            push!(else_body.args, elem)
            if isa(elem, LabelNode) && elem.label == goto.label # found else end
                push!(if_expr.args, _remove_goto(else_body.args))
                return true, if_expr, i
            end
        end
    else
        return true, if_expr, labelpos+1
    end
    error("If with goto found (else), but goto label not found. Unless: $unless \n ifbody: $ifbody")
end
"""
Handles a branch (`unless`/`gotoifnot`) at `unless_idx` in `list`.
It will return the corresponding controle flow `Expr` (e.g. `while`/`if`/`else`) and the
new position in the list.
"""
function handle_unless(list, unless_idx)
    unless_body = Expr(:block)
    unless_goto = list[unless_idx].args[2]
    pos = unless_idx + 1
    for i=pos:length(list)
        elem = list[i]
        if isa(elem, LabelNode) && elem.label == unless_goto
            isloop, loopexpr, i = handle_loop(list, pos-1, i, unless_body)
            isloop && return loopexpr, i
            isif, ifexpr, i = handle_if(list, pos-1, i, unless_body)
            isif && return ifexpr, i
        else
            push!(unless_body.args, elem)
        end
    end
end
"""
Replaces goto's with control flow statements while leaving labels/gotos in the AST.
This makes the different steps of processing easier.
"""
function _remove_goto(list)
    # (╯°□°）╯︵ ┻━┻
    result = Expr(:block); i = 1
    while i <= length(list)
        elem = list[i]
        expr = if isa_unless(elem)
            expr, i = handle_unless(list, i)
            expr
        else
            i += 1; elem
        end
        push!(result.args, expr)
    end
    result
end
"""
Returns a new expression block with all goto statements replaced by the corresponding
control flow `Expr` (e.g. `while`/`if`/`else`).
"""
function remove_goto(list)
    expr_nogoto = _remove_goto(list)
    remove_labelgoto!(expr_nogoto)
    expr_nogoto
end
