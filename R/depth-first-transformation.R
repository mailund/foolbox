#' Transform an expression.
#'
#' Traverses the expression `expr` depth-first and transform it using `callbacks`.
#'
#' @param expr      An R expression
#' @param callbacks List of callbacks to apply.
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return A modified expression.
#'
#' @seealso callbacks
#' @seealso identity_callback
#' @seealso depth_first_rewrite_function
#' @export
depth_first_rewrite_expr <- function(expr, callbacks, ...) {
    if (rlang::is_atomic(expr)) {
        return(callbacks$atomic(expr, ...))
    }
    if (rlang::is_pairlist(expr)) {
        return(callbacks$pairlist(expr, ...))
    }
    if (rlang::is_symbol(expr)) {
        return(callbacks$symbol(expr, ...))
    }
    if (rlang::is_primitive(expr)) {
        return(callbacks$primitive(expr, ...))
    }

    stopifnot(rlang::is_lang(expr))
    call_args <- rlang::call_args(expr)
    for (i in seq_along(call_args)) {
        expr[[i + 1]] <- depth_first_rewrite_expr(
            call_args[[i]], callbacks, ...
        )
    }
    callbacks$call(expr, ...)
}

#' Transform the body of function.
#'
#' Traverses the body of `fn` and rewrite it based on `callbacks`.
#'
#' @param fn        A (closure) function.
#' @param callbacks List of callbacks to apply.
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return A new function similar to `fn` but with a transformed body.
#'
#' @examples
#' log_calls_transformation <- function(call_expr, env, param, ...) {
#'     call_fn <- eval(call_expr[[1]], envir = env)
#'     if (rlang::is_primitive(call_fn)) return(call_expr)
#'     if (!rlang::is_symbol(call_expr[[1]])) return(call_expr)
#'
#'     rlang::expr({
#'         cat("Calling", rlang::UQ(as.character(call_expr[[1]])), "\n")
#'         rlang::UQ(call_expr)
#'     })
#' }
#'
#' f <- function(x) {
#'     if (x > 0) f(x - 1)
#'     else print("Done")
#' }
#'
#' cb <- callbacks() %>% with_call_callback(log_calls_transformation)
#' f <- depth_first_rewrite_function(f, cb)
#'
#' @seealso depth_first_rewrite_expr
#' @seealso callbacks
#' @export
depth_first_rewrite_function <- function(fn, callbacks, ...) {
    body(fn) <- depth_first_rewrite_expr(
        body(fn), callbacks,
        env = environment(fn), params = formals(fn),
        ...
    )
    fn
}
