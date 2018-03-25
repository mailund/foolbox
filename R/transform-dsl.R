
#' Functions for applying a sequence of rewrites.
#'
#' The [rewrite()] function applies a series of transformations to an input
#' function, `fn` and returns the result. This result can then be used in a
#' pipeline of [rewrite_with()] calls for further analysis.
#'
#' The flow of transformations goes starts with [rewrite()] and is followed by a
#' series of [rewrite_with()] for additional rewrite callbacks. For analysis, it
#' starts with [analyse()] and is followed by a pipeline of [analyse_with()].
#'
#'
#' @param fn The function to rewrite
#' @param expr When invoked on expressions, in [rewrite_expr()], the expression
#'   to rewrite.
#' @param callbacks The callbacks that should do the rewriting
#' @param ... Additional parameters passed along to the callbacks.
#'
#' @return A rewritten function
#'
#' @seealso rewrite_callbacks
#'
#' @examples
#' f <- function(x) 2 + x
#' cb <- rewrite_callbacks() %>%
#'    add_call_callback(f, function(expr, ...) {
#'        quote(2 + x)
#'    })
#' tr_f <- . %>% rewrite() %>% rewrite_with(cb)
#'
#' g <- function(y) y + f(y) # body(g) is now quote(y + (2 + x))
#'
#' @describeIn rewrite_with Apply `callbacks` over `fn` to rewrite it.
#' @export
rewrite_with <- function(fn, callbacks, ...)
    depth_first_rewrite_function(fn, callbacks, ...)

#' @describeIn rewrite_with Function for starting a rewrite.
#' @export
rewrite <- function(fn) fn %>% annotate_bound_symbols_in_function()

#' @describeIn rewrite_with Function for running analysis callbacks
#' @export
analyse <- rewrite # it is actually the same preprocessing

#' @describeIn rewrite_with Apply `callbacks` over `fn` to analyse it.
#' @export
analyse_with <- function(fn, callbacks, ...)
    depth_first_analyse_function(fn, callbacks, ...)

#' @describeIn rewrite_with Expression version of [rewrite()]
#' @export
rewrite_expr <- function(expr) expr

#' @describeIn rewrite_with Expression version of [rewrite_with()]
#' @export
rewrite_expr_with <- function(expr, callbacks, ...) {
    expr %>% depth_first_rewrite_expr(
        callbacks, ...
    )
}

#' @describeIn rewrite_with Expression version of [analyse()]
#' @export
analyse_expr <- function(expr) expr

#' @describeIn rewrite_with Expression version of [analyse_with()]
#' @export
analyse_expr_with <- function(expr, callbacks, ...) {
    expr %>% depth_first_analyse_expr(
        callbacks, ...
    )
}

#' Object for setting up a transformation pipeline when defining functions
#'
#' @export
rewrites <- structure(NA, class = "foolbox_rewrite_spec")

#' Provide list of rewrite transformations.
#'
#' This subscript operator is used together with \code{\link{rewrites}} to specify
#' a sequence of transformations to apply to a new function we define.
#'
#' @param dummy The dummy-table \code{\link{rewrites}}. It is only here because it
#'     allows us to use subscripts as part of the domain-specific language.
#' @param ... A list of rewrite functions.
#'
#' @export
`[.foolbox_rewrite_spec` <- function(dummy, ...) {
    transformation_exprs <- rlang::enexprs(...)
    transformations <- Map(function(trex) eval(rlang::expr(. %>% !!trex)),
                           transformation_exprs)
    structure(transformations, class = "foolbox_pipe")
}

#' This operator is used together with \code{\link{rewrites}} to transform a function
#' after it is defined and before it is assigned to a name.
#' @param pipe A specificiation of a a pipeline of transformations provided
#'     using the subscript operator to `rewrites`.
#' @param fn The function we wish to transform.
#'
#' @export
`<.foolbox_pipe` <- function(pipe, fn) {
    for (trans in pipe) {
        fn <- trans(fn)
    }
    fn
}
