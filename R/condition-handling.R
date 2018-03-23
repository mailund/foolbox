
#' Collection of warning flags used when traversing expressions.
#'
#' These are flags for turning warnings on or off when traversing expression
#' trees.
#'
#' The flags can be provided to transformation and analysis functions, and be
#' set or unset by the `set_`/`unset_` functions. The meaning of the flags are:
#'
#' - **warn_on_unknown_function:** If you have installed a callback with
#' [add_call_callback()] or [add_topdown_callback()], the traversal code will
#' check if a given call is to a known function installed by one of these. If
#' the function name of a call is not recognised as a function parameter or a
#' local variable, as annotated with [annotate_bound_symbols_in_function()],
#' then the code will issue a warning if this flag is set. The warning behaviour
#' depends on whether [annotate_bound_symbols_in_function()] has analysed the
#' function. If it hasn't, then we only consider function parameters as local
#' variables. If it has, we have more information about the local variables, so
#' we can make the warnings more accurate. The flag is set by default.
#'
#' - **warn_on_local_function:** If you have installed a callback with
#' [add_call_callback()] or [add_topdown_callback()], the traversal code will
#' check if a given call is to a known function installed by one of these. If
#' you have installed a function that has a name-clash with a local variable,
#' and this flag is set, then you will get a warning. If you have annotated the
#' expression tree using [annotate_bound_symbols_in_function()], then the
#' warning will be invoked both on local variables and function parameters; if
#' you have not annotated the expression tree, then it will only be invoked on
#' function arguments. The flag is set by default.
#'
#' Since R is a very dynamic language, it is not possible to know which local
#' variables might refer to functions and which do not -- and R will look for
#' functions if a variable is used as a call and potentially skip past a local
#' variable that refers to a non-function -- so the warnings are based on
#' heuristics in identifying local variables and are conservative in the sense
#' that they assume that if a call is to a name that matches a local variable,
#' then it is the local variable that is being called.
#'
#' @param flags Used when setting or unsetting flags.
#'
#' @export
warning_flags <- function() {
    list(
        warn_on_unknown_function = TRUE,
        warn_on_local_function = TRUE
    )
}

make_set_warning <- function(warning_name, val) {
    force(warning_name)
    function(flags) {
        flags[[warning_name]] <- val
        flags
    }
}

#' @describeIn warning_flags Enable warnings when encountering an unknown
#'   function
#' @export
set_warn_on_unknown_function <-
    make_set_warning("warn_on_unknown_function", TRUE)

#' @describeIn warning_flags Disable warnings when encountering an unknown
#'   function
#' @export
unset_warn_on_unknown_function <-
    make_set_warning("warn_on_unknown_function", FALSE)

#' @describeIn warning_flags Enable warnings when encountering a local variable
#'   with a name that matches one installed for transformation.
#' @export
set_warn_on_local_function <-
    make_set_warning("warn_on_local_function", TRUE)

#' @describeIn warning_flags Disable warnings when encountering a local variable
#'   with a name that matches one installed for transformation.
#' @export
unset_warn_on_local_function <-
    make_set_warning("warn_on_local_function", FALSE)
