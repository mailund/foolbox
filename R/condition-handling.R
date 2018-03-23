
# FIXME: Document the warning flags id:16 gh:40 ic:gh
#' Collection of warning flags used when traversing expressions.
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

#' @describeIn warning_flags Enable warnings when encountring an unknown
#'   function
#' @export
set_warn_on_unknown_function <-
    make_set_warning("warn_on_unknown_function", TRUE)

#' @describeIn warning_flags Disable warnings when encountring an unknown
#'   function
#' @export
unset_warn_on_unknown_function <-
    make_set_warning("warn_on_unknown_function", FALSE)

#' @describeIn warning_flags Enable warnings when encountring a local variable
#'   with a name that matches one installed for transformation.
#' @export
set_warn_on_local_function <-
    make_set_warning("warn_on_local_function", TRUE)

#' @describeIn warning_flags Disable warnings when encountring a local variable
#'   with a name that matches one installed for transformation.
#' @export
unset_warn_on_local_function <-
    make_set_warning("warn_on_local_function", FALSE)
