## Transformation functions #################################################

#' Transform an expression.
#'
#' Traverses the expression `expr` depth-first and transform it using `callbacks`.
#'
#' @param expr      An R expression
#' @param callbacks List of callbacks to apply.
#' @param topdown   A list of additional information gathered in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return A modified expression.
#'
#' @seealso rewrite_callbacks
#' @seealso identity_rewrite_callback
#' @seealso depth_first_rewrite_function
#' @export
depth_first_rewrite_expr <- function(expr, callbacks, topdown, wflags,
                                     ...) {
    if (rlang::is_atomic(expr)) {
        return(callbacks$atomic(
            expr,
            topdown = topdown, wflags = wflags, ...
        ))
    }
    if (rlang::is_pairlist(expr)) {
        return(callbacks$pairlist(
            expr,
            topdown = topdown, wflags = wflags, ...
        ))
    }
    if (rlang::is_symbol(expr)) {
        return(callbacks$symbol(
            expr,
            topdown = topdown, wflags = wflags, ...
        ))
    }
    if (rlang::is_primitive(expr)) {
        return(callbacks$primitive(
            expr,
            topdown = topdown, wflags = wflags, ...
        ))
    }

    stopifnot(rlang::is_lang(expr))
    # Use callCC to be able to skip an evaluation based on topdown analysis
    callCC(function(escape) {
        skip <- function() escape(expr) # skip means leaving the body unchanged
        # collect topdown info.
        topdown <- callbacks$topdown(
            expr,
            topdown = topdown, wflags = wflags, skip = skip, ...
        )

        # handle depth first
        call_args <- rlang::call_args(expr)
        for (i in seq_along(call_args)) {
            expr[[i + 1]] <- depth_first_rewrite_expr(
                call_args[[i]], callbacks,
                topdown = topdown, wflags = wflags,
                ...
            )
        }

        # then handle the actual call
        callbacks$call(
            expr,
            topdown = topdown, wflags = wflags, ...
        )
    })
}

#' Transform the body of function.
#'
#' Traverses the body of `fn` and rewrite it based on `callbacks`.
#'
#' @param fn        A (closure) function.
#' @param callbacks List of callbacks to apply.
#' @param topdown   A list of additional information that will be considered
#'                  top-down in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
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
#' cb <- rewrite_callbacks() %>% with_call_callback(log_calls_transformation)
#' f <- depth_first_rewrite_function(f, cb)
#'
#' @seealso depth_first_rewrite_expr
#' @seealso rewrite_callbacks
#' @export
depth_first_rewrite_function <- function(fn, callbacks,
                                         topdown = list(),
                                         wflags = warning_flags(),
                                         ...) {
    body(fn) <- depth_first_rewrite_expr(
        body(fn), callbacks,
        env = environment(fn), params = formals(fn),
        topdown = topdown, wflags = wflags,
        ...
    )
    fn
}

## Analysis functions #################################################

#' Analyse an expression.
#'
#' Traverses the expression `expr` depth-first and analyse it it using `callbacks`.
#'
#' @param expr      An R expression
#' @param callbacks List of callbacks to apply.
#' @param topdown   A list of additional information gathered in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return The result of the last bottom-up traversal.
#'
#' @seealso analysis_callbacks
#' @seealso identity_analysis_callback
#' @seealso depth_first_analyse_function
#' @export
depth_first_analyse_expr <- function(expr, callbacks, topdown, wflags,
                                     ...) {
    if (rlang::is_atomic(expr)) {
        return(callbacks$atomic(
            expr,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }
    if (rlang::is_pairlist(expr)) {
        return(callbacks$pairlist(
            expr,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }
    if (rlang::is_symbol(expr)) {
        return(callbacks$symbol(
            expr,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }
    if (rlang::is_primitive(expr)) {
        return(callbacks$primitive(
            expr,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }

    stopifnot(rlang::is_lang(expr))
    # Use callCC to be able to skip an evaluation based on topdown analysis
    callCC(function(escape) {
        # skip means returning no bottomup info.
        skip <- function() escape(list())
        topdown <- callbacks$topdown(
            expr, wflags = wflags,
            topdown = topdown, skip = skip, ...
        )

        # handle depth first
        call_args <- rlang::call_args(expr)
        bottomup <- vector("list", length = length(call_args))
        for (i in seq_along(call_args)) {
            bottomup[[i]] <- depth_first_analyse_expr(
                call_args[[i]], callbacks,
                wflags = wflags,
                topdown = topdown,
                ...
            )
        }

        # then handle the actual call
        callbacks$call(
            expr, topdown = topdown, bottomup = bottomup, wflags = wflags, ...
        )
    })
}

#' Analyse the body of function.
#'
#' Traverses the body of `fn` and analyse it based on `callbacks`.
#'
#' @param fn        A (closure) function.
#' @param callbacks List of callbacks to apply.
#' @param topdown   A list of additional information that will be considered
#'                  top-down in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return The result of the last bottom-up call to a callback.
#'
#' @seealso depth_first_analyse_expr
#' @seealso depth_first_rewrite_function
#' @seealso analysis_callbacks
#' @export
depth_first_analyse_function <- function(fn, callbacks,
                                         topdown = list(),
                                         wflags = warning_flags(),
                                         ...) {
    depth_first_analyse_expr(
        body(fn), callbacks,
        env = environment(fn), params = formals(fn),
        topdown = topdown,
        ...
    )
}
