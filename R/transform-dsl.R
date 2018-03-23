
#' Functions for applying a sequence of rewrites.
#'
#' The [rewrite()] function applies a series of transformations to an input
#' function, `fn` and returns the result. This result can then be used in a
#' pipeline of [rewrite_with()] calls for further analysis.
#'
#' The flow of transformations goes starts with [rewrite()] and is followed
#' by a series of [rewrite_with()] for additional rewrite callbacks.
#'
#' @param fn The function to rewrite
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
