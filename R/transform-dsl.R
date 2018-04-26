
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
#' This functions will annotate a function's body with two attributes for each
#' sub-expression in the body. Each `call` expression in the body will be
#' annotated with these two attributes:
#'
#'   -  **assigned_symbols**: Variables that appear to the left of an
#'         assignment in a sub-expression of the call that is likely to
#'         affect the scope of the call.
#'
#'   - **bound**: Variables that are either assigned to, thus potentially
#'         local in the scope, or function parameters from an enclosing scope,
#'         which will definitely be bound at this position.
#'
#' Since R does not require that we declare local variables, and since the
#' variables that are assigned to a local scope depend on the runtime execution
#' of functions, we cannot determine with any certainty which variables will be
#' assigned to in any given scope at any given program point. So the best we can
#' do is figure out which variables are *potentially* assigned to. Which is what
#' this function does.
#'
#' The rules for when we are assigning to a local variable are a bit
#' complicated. For control structures, we can assume that assignments will
#' be to the local scope. People can change the implementation of these so it
#' isn't, but then they are only hurting themselves and deserve the extra
#' pain we can give them. For other call arguments, it gets a little more
#' complicated. With standard-evaluation, if we have an arrow assignment in a
#' function argument, then the assignment happens in the calling scope. So we
#' will assume this happens unless we are handling cases we know have NSE,
#' such as `with`. If an assignment is inside a block, however, we will
#' assume that NSE *is* in play, by default, and not consider it a local
#' assignment.
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
#' g <- function(y) y + f(y)
#' tr_f(g)
#'
#' collect_symbols <- function(expr, ...) {
#'    list(symbols = as.character(expr))
#' }
#' callbacks <- analysis_callbacks() %>% with_symbol_callback(collect_symbols)
#' f %>% analyse() %>% analyse_with(callbacks)
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
#' @examples
#' # This is a very simple inline function that require we
#' # provide the function body as it should be inserted.
#' # For a more detailed version, see the Tutorial vignette.
#' # For a version that permits partial evaluation, see that vignette.
#' inline <- function(f, fn, body) {
#'    body <- substitute(body)
#'    rewrite(f) %>%
#'      rewrite_with(
#'          rewrite_callbacks() %>%
#'            add_call_callback(fn, function(expr, ...) body)
#'      )
#' }
#'
#' g <- function(x) x**2
#' h <- rewrites[inline(g,y**2)] < function(y) y + g(y)
#' h
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
#' @seealso `<.foolbox_pipe`
#' @seealso rewrites
#'
#' @examples
#' # This is a very simple inline function that require we
#' # provide the function body as it should be inserted.
#' # For a more detailed version, see the Tutorial vignette.
#' inline <- function(f, fn, body) {
#'    body <- substitute(body)
#'    rewrite(f) %>%
#'      rewrite_with(
#'          rewrite_callbacks() %>%
#'            add_call_callback(fn, function(expr, ...) body)
#'      )
#' }
#'
#' g <- function(x) x**2
#' h <- rewrites[inline(g,y**2)] < function(y) y + g(y)
#' h
#'
#' @export
`[.foolbox_rewrite_spec` <- function(dummy, ...) {
    transf_exprs <- rlang::enexprs(...)
    transf_env <- rlang::caller_env()
    transf <- Map(function(trex)
                    eval(rlang::expr(function(fn) fn %>% !!trex), transf_env),
                  transf_exprs)
    structure(transf, class = "foolbox_pipe")
}

#' This operator is used together with \code{\link{rewrites}} to transform a function
#' after it is defined and before it is assigned to a name.
#' @param pipe A specificiation of a a pipeline of transformations provided
#'     using the subscript operator to [rewrites()].
#' @param fn The function we wish to transform.
#'
#' @examples
#' # This is a very simple inline function that require we
#' # provide the function body as it should be inserted.
#' # For a more detailed version, see the Tutorial vignette.
#' inline <- function(f, fn, body) {
#'    body <- substitute(body)
#'    rewrite(f) %>%
#'      rewrite_with(
#'          rewrite_callbacks() %>%
#'            add_call_callback(fn, function(expr, ...) body)
#'      )
#' }
#'
#' g <- function(x) x**2
#' h <- rewrites[inline(g,y**2)] < function(y) y + g(y)
#' h
#'
#' @seealso `[.foolbox_rewrite_spec`
#' @seealso rewrites
#' @export
`<.foolbox_pipe` <- function(pipe, fn) {
    for (trans in pipe) {
        fn <- trans(fn)
    }
    fn
}
