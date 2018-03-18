#' A callback that does not do any transformation.
#'
#' @param expr   The expression to (not) transform.
#' @param env    The environment of the function the expression is inside.
#' @param params The formal parameters of the function.
#' @return `expr`
#' @export
identity_callback <- function(expr, env, params) expr

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
#' @export
# I'm using a function here, although it would be more natural to just use the
# value, because somehow the function identity gets messed up in covr
callbacks <- function() list(
        atomic = identity_callback,
        pairlist = identity_callback,
        symbol = identity_callback,
        primitive = identity_callback,
        call = identity_callback
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
