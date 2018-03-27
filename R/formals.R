#' Remove a parameter from the formal parameters of a function.
#'
#' @param fn  A function we are modifying
#' @param par A parameter of `fn` (should be in `formals(fn)` and be quoted)
#' @return A modified function equal to `fn` but with `par` removed from the formal
#'         parameters.
#' @export
remove_formal_ <- function(fn, par) {
    par_name <- as.character(par)
    params <- formals(fn)
    param_names <- names(params)
    formals(fn) <- params[param_names != par_name]
    fn
}

#' Remove a parameter from the formal parameters of a function.
#'
#' @param fn  A function we are modifying
#' @param par A parameter of `fn` (should be in `formals(fn)` and not be quoted)
#' @return A modified function equal to `fn` but with `par` removed from the formal
#'         parameters.
#' @export
remove_formal <- function(fn, par) {
    remove_formal_(fn, rlang::enexpr(par))
}
