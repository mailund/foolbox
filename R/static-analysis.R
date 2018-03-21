
## Functions for extracting information about functions and expressions

collection_callback <- function(expr, bottomup, ...) {
    bottomup <- merge_bottomup(bottomup)

    # This function is installed to be called on assignments and
    # on for-loops (where there is an implicit assignment to the
    # iterator variable)
    if (expr[[1]] == "<-") {
        if (rlang::is_symbol(expr[[2]])) {
            local_var <- as.character(expr[[2]])
            bottomup$locals <- c(local_var, bottomup$locals)
        }
    } else if (expr[[1]] == "for") {
        local_var <- as.character(expr[[2]])
        bottomup$locals <- c(local_var, bottomup$locals)
    } else {
        stop("Unexpected function call!")
    }

    bottomup
}

skip_independent_scopes_callback <- function(expr, skip, ...) {
    skip()
}

collect_assigned_symbols_callbacks <- analysis_callbacks() %>%
    add_call_callback(`<-`, collection_callback) %>%
    add_call_callback(`for`, collection_callback) %>%
    add_topdown_callback(`function`, skip_independent_scopes_callback) %>%
    add_topdown_callback(with, skip_independent_scopes_callback)

#' Extracts all the symbols that appear on the left-hand side of an
#' assignment.
#'
#' Since R does not require that we declare local variables, and since
#' the variables that are assigned to a local scope depend on the runtime
#' execution of functions, we cannot determine with any certainty which
#' variables will be assigned to in any given scope at any given program
#' point. So the best we can do is figure out which variables are
#' *potentially* assigned to. Which is what this function does.
#'
#' The [collect_assigned_symbols_in_function()] function reformats the collected
#' data into a character vector, removes duplications, and remove the
#' formal parameters of the function from the list, so those are not considered
#' local variables (rather, they are considered formals and presumably handled
#' elsewhere as such).
#'
#' @param expr    The expression to analyse
#' @param fun     The function whose body we should analyse
#' @param env     Environment in which to look up symbols.
#' @param params  Parameters for the function being analysed (if
#'                these are needed).
#' @param topdown Information to pass top-down in the traversal.
#'
#' @return A list containing the symbols that were assigned to.
#'
#' @describeIn collect_asigned_symbols_in_expression Analyse an expression.
#' @export
collect_assigned_symbols_in_expression <- function(expr, env,
                                                   params = list(),
                                                   topdown = list()) {
    depth_first_analyse_expr(
        expr,
        callbacks = collect_assigned_symbols_callbacks,
        env = env, params = alist(),
        topdown = topdown
    )
}

#' @describeIn collect_asigned_symbols_in_expression
#'             Analyse the body of a function.
#' @export
collect_assigned_symbols_in_function <- function(fun, topdown = list()) {
    res <- depth_first_analyse_function(
        fun, collect_assigned_symbols_callbacks,
        topdown = topdown
    )
    params <- names(formals(fun))

    locals <- unlist(
        Filter(
            function(var) !(var %in% params),
            unique(res$locals)
        )
    )
    if (is.null(locals)) character() else locals
}
