#' A callback that does not do any transformation.
#'
#' @param expr  The expression to (not) transform.
#' @param evn   The environment of the function the expression is inside.
#' @param param The formal parameters of the function.
#' @return `expr`
#' @export
identity_callback <- function(expr, env, param) expr

#' Default expression-transformation callbacks.
#'
#' @seealso with_atomic_callback
#' @seealso with_pairlist_callback
#' @seealso with_symbol_callback
#' @seealso with_primitive_callback
#' @seealso with_call_callback
#' @export
callbacks <- list(
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
make_with_callback <- function(cb_name) {
    force(cb_name)
    function(callbacks, fn) {
        callbacks[[cb_name]] <- fn
        callbacks
    }
}

#' @describeIn callbacks Set the atomic callback function.
#' @export
with_atomic_callback <- make_with_callback('atomic')
#' @describeIn callbacks Set the pairlist callback function.
#' @export
with_pairlist_callback <- make_with_callback('pairlist')
#' @describeIn callbacks Set the symbol callback function.
#' @export
with_symbol_callback <- make_with_callback('symbol')
#' @describeIn callbacks Set the primitive callback function.
#' @export
with_primitive_callback <- make_with_callback('primitive')
#' @describeIn callbacks Set the call callback function.
#' @export
with_call_callback <- make_with_callback('call')

