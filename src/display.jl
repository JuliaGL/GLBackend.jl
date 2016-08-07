export glscreen



let screen_list = Screen[]
    global current_screen, add_screen, get_screens, empty_screens!
    current_screen() = last(screen_list)
    add_screen(screen) = push!(screen_list, screen)
    get_screens() = copy(screen_list)
    empty_screens!() = empty!(screen_list)
end


function cleanup_old_screens()
    for screen in get_screens()
        destroy!(screen)
    end
    empty!(timer_signal_dict)
    empty_screens!()
    GLFW.Terminate()
    GLFW.Init()
    reset_texture_atlas!()
    GLAbstraction.empty_shader_cache!()
end

function glscreen(name="GLVisualize";
        resolution = GLWindow.standard_screen_resolution(),
        debugging = false,
        background = RGBA(1,1,1,1)
    )

    cleanup_old_screens()

    screen = Screen(name, resolution=resolution, debugging=debugging, color=background)
    add_screen(screen)

    GLWindow.add_complex_signals!(screen) #add the drag events and such
    add_oit_fxaa_postprocessing!(screen) # add postprocessing

    screen
end

const timer_signal_dict = Dict{Int, WeakRef}()
"""
Creates a timer signal with `updates_per_second` while `window` is open.
It's reusing timer signals with the same update rate and registering the updates
with GLFW.
"""
function get_timer_signal(updates_per_second, window=current_screen())
    signal = get!(timer_signal_dict, updates_per_second) do
        # because this is a function, it'll only get executed if needed
        WeakRef(fpswhen(window.inputs[:window_open], updates_per_second))
    end.value
    signal
end

function fold_loop(v0, _)
    val, range, index = v0
    val = range[index]
    index += 1
    index>length(range) && (index = 1)
    (val, range, index)
end

function loop(range::Range, rate=60)
    t = get_timer_signal(rate)
    map(first, foldp(fold_loop, (first(range), range, 1), t))
end


function fold_bounce(v0, _)
    val, range, index, direction = v0
    val = range[index]
    index += direction
    if index in (length(range)+1, 0)
        direction = -direction
        index += 2direction
    end
    (val, range, index, direction)
end

function bounce{T}(range::Range{T}, rate=60)
    t = get_timer_signal(rate)
    map(first, foldp(fold_bounce, (first(range), range, 1, 1), t))
end


export bounce, loop





"""
Materializes the `Visualization` on the an OpenGL `Screen`, transfering everything
to the GPU and adding it to the windows renderlist.
"""
function push!(window::Screen, v::Visualization)
    visual_specific!(v)
    glp = get(v, :gl_primitive, GL_TRIANGLES)
    pre = get(v, :prerender, GLAbstraction.EmptyPrerender())
    # assert opacity is defined
    get!(v, :is_fully_opaque) do
        isopaque(get_color(v))
    end
    if haskey(v, :instances)
        robj = instanced_renderobject(
            v.parameters, shader,
            bb, glp, v[:instances], pre=pre
        )
    else
        robj = std_renderobject(v.parameters, shader, bb, glp, pre=pre)
    end
    if haskey(v, :postrender)
        tmp = robj.postrenderfunction # needs to be always executed
        pr = v[:postrender] # for cleaning up!
        robj.postrenderfunction = () -> (tmp(); pr())
    end
    for child in v.children
        push!(window, child, camera)
    end
    push!(window, robj, v.camera)
end
