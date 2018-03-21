#' A callback that does not do any transformation.
#'
#' Callbacks have one required argument, `expr`, but will actually
#' be called with more. The additional named parameters are:
#' - **env**      The function environment of the function we are transforming
#' - **params**   The formal parameters of the function we are transforming
#' - **topdown**  Data passed top-down in the traversal.
#' - **buttomup** Data collected by depth-first traversals before a callback
#'                is called.
#' plus whatever the user provide to [depth_first_rewrite_function()].
#'
#' @param expr   The expression to (not) transform.
#' @param ...    Additional named parameters.
#' @return `expr`
#' @export
identity_callback <- function(expr, ...) expr

#' Top-down analysis callback.
#'
#' @param expr    The expression before we modify it.
#' @param topdown Information from further up the expression tree.
#' @param skip    An escape function. If called, the transformation
#'                or analysis traversal will skip this expression
#'                and continue at the sibling level.
#' @param ...     Additional data that might be passed along
#' @return Updated `topdown` information.
#' @export
nop_topdown_callback <- function(expr, topdown, skip, ...) topdown

# FIXME: better documentation for the callbacks.

#' Default expression-transformation callbacks.
#'
#' Callbacks must be functions that take three arguments: The expression
#' to rewrite, the environment of the function we are rewriting (i.e. the
#' environment it is defined in, not the function call frame), and a list of
#' formal parameters of the function we are translating.
#'
#' @param callbacks The list of callbacks
#' @param fn        A function to install as a callback.
#'
#' @seealso with_atomic_callback
#' @seealso with_pairlist_callback
#' @seealso with_symbol_callback
#' @seealso with_primitive_callback
#' @seealso with_call_callback
#' @seealso with_topdown_callback
#' @export
# I'm using a function here, although it would be more natural to just use the
# value, because somehow the function identity gets messed up in covr
callbacks <- function() list(
        atomic = identity_callback,
        pairlist = identity_callback,
        symbol = identity_callback,
        primitive = identity_callback,
        call = identity_callback,
        topdown = nop_topdown_callback
    )

#' Create a function for setting callbacks.
#'
#' @param cb_name The name of the callback to set
#' @return A function that can be used in a pipe to set a callback.
# I disable coverage here since it only tracks the closure body
# and not the actual function in the tests (the function is called)
# before the tests are run, when the package is built.
# nocov start
make_with_callback <- function(cb_name) {
    force(cb_name)
    function(callbacks, fn) {
        callbacks[[cb_name]] <- fn
        callbacks
    }
}
# nocov end

#' @describeIn callbacks Set the atomic callback function.
#' @export
with_atomic_callback <- make_with_callback("atomic")
#' @describeIn callbacks Set the pairlist callback function.
#' @export
with_pairlist_callback <- make_with_callback("pairlist")
#' @describeIn callbacks Set the symbol callback function.
#' @export
with_symbol_callback <- make_with_callback("symbol")
#' @describeIn callbacks Set the primitive callback function.
#' @export
with_primitive_callback <- make_with_callback("primitive")
#' @describeIn callbacks Set the call callback function.
#' @export
with_call_callback <- make_with_callback("call")
#' @describeIn callbacks Set the topdown information passing callback function.
#' @export
with_topdown_callback <- make_with_callback("topdown")

#' Add a function-specific callback to the call callbacks.
#'
#' This function adds to the existing call callback, rather than replace it,
#' by putting a callback in front of it to be tested first. The callback will
#' be invoked when the traversal sees a call to a specific function.
#'
#' @param callbacks The existing callbacks.
#' @param fn        The function to which calls should be modified.
#' @param cb        The callback function to invoke.
#'
#' @return          The updated callbacks.
#' @export
add_call_callback <- function(callbacks, fn, cb) {
    next_cb <- callbacks$call
    force(fn)
    force(cb)
    closure <- function(call_expr, env, params, ...) {
        # make sure the call is not to a local variable--if it is,
        # we can't evaluate it at transformation time. We propagate
        # to the next callback.
        call_name <- call_expr[[1]]
        if (as.character(call_name) %in% names(params)) {
            return(next_cb(call_expr, env = env, params = params, ...))
        }

        # now try to get the actual function by evaluating it
        err_fun <- function(e) {
            warning(paste0(
                "The function ", as.character(call_name),
                " could not be evaluated to an actual funcion in ",
                "this scope."
            ))
            NULL
        }
        fun <- tryCatch(eval(call_name, env), error = err_fun)
        if (!is.null(fun) && identical(fun, fn)) {
            return(cb(call_expr, env, params, ...))
        } else {
            # default for closure: try the next in line
            next_cb(call_expr, env, params, ...)
        }
    }
    callbacks$call <- closure
    callbacks
}

# FIXME: Add an `add_topdown_callback`?
