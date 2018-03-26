
## Functions for extracting information about functions and expressions ####

collection_callback <- function(expr, bottomup, ...) {
    bottomup <- merge_bottomup(bottomup)

    # This function is installed to be called on assignments and
    # on for-loops (where there is an implicit assignment to the
    # iterator variable)
    if (expr[[1]] == "<-" || expr[[1]] == "=") {
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
    skip(list())
}

collect_assigned_symbols_callbacks <- analysis_callbacks() %>%
    add_call_callback(`<-`, collection_callback) %>%
    add_call_callback(`=`, collection_callback) %>%
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
collect_assigned_symbols_in_function <- function(fun, topdown = list()) {
    res <- depth_first_analyse_function(
        fun, collect_assigned_symbols_callbacks,
        topdown = topdown,
        wflags = warning_flags() %>% unset_warn_on_unknown_function()
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


## Functions annotating expressions #########################################

annotate_assigned_symbols_callback <- function(expr, next_cb, ...) {
    locals <- collect_from_args(expr, "assigned_symbols") %>%
        unlist() %>%
        unique()
    if (is.null(locals)) locals <- character()

    # This function is installed to be called on assignments and
    # on for-loops (where there is an implicit assignment to the
    # iterator variable)
    if (expr[[1]] == "<-" || expr[[1]] == "=") {
        if (rlang::is_symbol(expr[[2]])) {
            local_var <- as.character(expr[[2]])
            locals <- c(local_var, locals)
        }
    } else if (expr[[1]] == "for") {
        local_var <- as.character(expr[[2]])
        locals <- c(local_var, locals)
    } else {
        stop("Unexpected function call!")
    }

    attr(expr, "assigned_symbols") <- locals
    expr
}

propagate_assigned_symbols_callback <- function(expr, ...) {

    # FIXME: Make a handle so people can guide these id:13 gh:34 ic:gh
    # heuristics, e.g. tell when arguments are evaluated in the calling
    # scope and when they are evaluated in another and assignments
    # won't affect this scope.

    call_name <- as.character(expr[[1]])
    if (call_name %in% c("{", "if", "for", "while", "repeat")) {
        condition <- function(expr) TRUE
    } else if (call_name == "with") {
        condition <- function(expr) FALSE
    } else {
        condition <- function(expr) {
            rlang::is_lang(expr) && expr[[1]] == "<-"
        }
    }
    locals <- collect_from_args(expr, "assigned_symbols", condition) %>%
        unlist() %>%
        unique()

    if (is.null(locals)) locals <- character()
    attr(expr, "assigned_symbols") <- locals
    expr
}

# Called before recursions. Combines variables propagated down
# with new formals if we see a local function definition
collect_bound_variables_callback <- function(expr, topdown, ...) {
    bound <- c(topdown, attr(expr, "assigned_symbols"))
    if (expr[[1]] == "function") {
        bound <- c(names(expr[[2]]), bound)
    }
    bound # passed as `topdown` to the rewrite callback
}

propagate_bound_variables_callback <- function(expr, topdown, ...) {
    attr(expr, "bound") <- unique(topdown)
    expr
}

#' Put attribute "assigned_symbols" on expressions bottom-up
annotate_assigned_symbols_callbacks <- rewrite_callbacks() %>%
    with_call_callback(propagate_assigned_symbols_callback) %>%
    add_call_callback(`<-`, annotate_assigned_symbols_callback) %>%
    add_call_callback(`=`, annotate_assigned_symbols_callback) %>%
    add_call_callback(`for`, annotate_assigned_symbols_callback)

#' Propagate parameters and local variables top-down to assign attribute
#' "bound" to all call expressions.
annotate_bound_variables_callbacks <- rewrite_callbacks() %>%
    with_topdown_callback(collect_bound_variables_callback) %>%
    with_call_callback(propagate_bound_variables_callback)

#' Annotate sub-expressions with variables bound in their scope.
#'
#' Extracts all the symbols that appear on the left-hand side of an assignment
#' or as function parameters and annotate each sub-expression with those.
#'
#' This function will annotate a function's body with two attributes for each
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
#' @param fn The function whose body we should analyse
#'
#' @return A function who's expressions are annotated with potentially local
#'   variables.
annotate_bound_symbols_in_function <- function(fn) {
    fn %>% depth_first_rewrite_function(
        annotate_assigned_symbols_callbacks,
        wflags = warning_flags() %>% unset_warn_on_unknown_function()
    ) %>% depth_first_rewrite_function(
        annotate_bound_variables_callbacks,
        topdown = names(formals(fn))
    )
}
