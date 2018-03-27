## Transformation functions #################################################

#' Transform an expression.
#'
#' Traverses the expression `expr` depth-first and transform it using `callbacks`.
#'
#' @param expr      An R expression
#' @param callbacks List of callbacks to apply.
#' @param params    Parameters of the function we are rewriting. If we are
#'   working on a raw expression, just use the default, which is an empty list.
#' @param topdown   A list of additional information gathered in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return A modified expression.
#'
#' @seealso rewrite_callbacks
#' @seealso identity_rewrite_callback
#' @seealso depth_first_rewrite_function
depth_first_rewrite_expr <- function(expr, callbacks,
                                     params = list(),
                                     topdown = list(),
                                     wflags = warning_flags(),
                                     ...) {
    if (rlang::is_atomic(expr)) {
        return(callbacks$atomic(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, ...
        ))
    }
    if (rlang::is_primitive(expr)) {
        return(callbacks$primitive(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, ...
        ))
    }
    if (rlang::is_symbol(expr)) {
        return(callbacks$symbol(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, ...
        ))
    }

    stopifnot(rlang::is_lang(expr) || rlang::is_pairlist(expr))
    # Use callCC to be able to skip an evaluation based on topdown analysis
    callCC(function(skip) {

        if (rlang::is_pairlist(expr)) {
            # collect topdown info.
            topdown <- callbacks$topdown_pairlist(
                expr,
                params = params,
                topdown = topdown, wflags = wflags,
                skip = skip, ...
            )

            # handle depth first
            for (i in seq_along(expr)) {
                expr[[i]] <- depth_first_rewrite_expr(
                    expr = expr[[i]],
                    callbacks = callbacks,
                    params = params,
                    topdown = topdown, wflags = wflags,
                    ...
                )
            }
            callbacks$pairlist(
                expr,
                params = params,
                topdown = topdown,
                wflags = wflags, ...
            )
        } else {
            # collect topdown info.
            topdown <- callbacks$topdown_call(
                expr,
                params = params,
                topdown = topdown, wflags = wflags, skip = skip, ...
            )

            # handle depth first
            expr[[1]] <- depth_first_rewrite_expr(
                expr[[1]], callbacks,
                params = params,
                topdown = topdown, wflags = wflags,
                ...
            )
            call_args <- rlang::call_args(expr)
            for (i in seq_along(call_args)) {
                expr[[i + 1]] <- depth_first_rewrite_expr(
                    call_args[[i]], callbacks,
                    params = params,
                    topdown = topdown, wflags = wflags,
                    ...
                )
            }

            callbacks$call(
                expr,
                params = params,
                topdown = topdown,
                wflags = wflags, ...
            )
        }

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
#' @seealso depth_first_rewrite_expr
#' @seealso rewrite_callbacks
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
#' Traverses the expression `expr` depth-first and analyse it it using
#' `callbacks`.
#'
#' @param expr      An R expression
#' @param callbacks List of callbacks to apply.
#' @param params    Parameters of the function we are analysing. If we are
#'   working on a raw expression, just use the default, which is an empty list.
#' @param topdown   A list of additional information gathered in the traversal.
#' @param wflags    Warning flags, see [warning_flags()].
#' @param ...       Additional data that will be passed along to callbacks.
#'
#' @return The result of the last bottom-up traversal.
#'
#' @seealso analysis_callbacks
#' @seealso identity_analysis_callback
#' @seealso depth_first_analyse_function
depth_first_analyse_expr <- function(expr, callbacks,
                                     params = list(),
                                     topdown = list(),
                                     wflags = warning_flags(),
                                     ...) {
    if (rlang::is_atomic(expr)) {
        return(callbacks$atomic(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }
    if (rlang::is_symbol(expr)) {
        return(callbacks$symbol(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }
    if (rlang::is_primitive(expr)) {
        return(callbacks$primitive(
            expr = expr,
            params = params,
            topdown = topdown, wflags = wflags, bottomup = list(), ...
        ))
    }


    stopifnot(rlang::is_lang(expr) || rlang::is_pairlist(expr))

    # Use callCC to be able to skip an evaluation based on topdown analysis
    callCC(function(skip) {
        if (rlang::is_pairlist(expr)) {
            topdown <- callbacks$topdown_pairlist(
                expr = expr,
                params = params, wflags = wflags,
                topdown = topdown, skip = skip, ...
            )

            # handle depth first
            bottomup <- vector("list", length = length(expr))
            for (i in seq_along(expr)) {
                bottomup[[i]] <- depth_first_analyse_expr(
                    expr[[i]], callbacks,
                    params = params,
                    wflags = wflags,
                    topdown = topdown,
                    ...
                )
            }
            callbacks$pairlist(
                expr,
                params = params,
                topdown = topdown,
                bottomup = bottomup,
                wflags = wflags,
                ...
            )
        } else {

            topdown <- callbacks$topdown_call(
                expr,
                params = params, wflags = wflags,
                topdown = topdown, skip = skip, ...
            )

            # handle depth first
            bottomup <- vector("list", length = length(expr))
            for (i in seq_along(expr)) {
                bottomup[[i]] <- depth_first_analyse_expr(
                    expr[[i]], callbacks,
                    params = params,
                    wflags = wflags,
                    topdown = topdown,
                    ...
                )
            }

            callbacks$call(
                expr,
                params = params,
                topdown = topdown,
                bottomup = bottomup,
                wflags = wflags, ...
            )
        }
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
