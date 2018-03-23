
#' Merge the results of several bottomup results.
#'
#' The `bottomup` parameter in callbacks will be calculated for all parameters
#' of `call`` expressions. The parameter to the callbacks are thus a list of
#' lists. This function merges these lists into one that contain a list for
#' each named component in the `bottomup` parameter. If results are not
#' named in the `bottomup` list, they are discarded.
#'
#' The vectors from `bottomup` are concatenated, so one level of lists
#' will be flattened. Use more lists, like `list(list(2), list(3))`
#' if you want to prevent this.
#'
#' @param bottomup List of bottom up analysis results.
#'
#' @seealso depth_first_analyse_function
#' @seealso depth_first_analyse_expr
#'
#' @export
merge_bottomup <- function(bottomup) {
    components <- lapply(bottomup, names) %>% unlist() %>% unique()
    result <- vector("list", length = length(components))
    names(result) <- components

    # processing in reverse order to keep the order of results
    # even though we are prepending to lists.
    for (sublist in rev(bottomup)) {
        for (name in names(sublist)) {
            result[[name]] <- c(sublist[[name]], result[[name]])
        }
    }

    result
}

#' Collect attributes set in the arguments to a call expression.
#'
#' Givcen a call expression `expr`, this function scans the arguments to the
#' call and extracts the attribute `attribute` from each where the
#' `condition` predicate evaluates to `TRUE`, and it concatenates all these.
#'
#' @param expr The call expression to process.
#' @param attribute The attribute we want to collect from the arguments.
#' @param condition A predicate. Only arguments where the condition evaluats to
#'     `TRUE` will be included in the result.
#' @return A list or vector obtained by concatenating the attributes from
#'     the arguments.
#' @export
collect_from_args <- function(expr, attribute,
                              condition = function(expr) TRUE) {
    collected <- list()
    args <- rlang::call_args(expr)
    for (a in args) {
        if (condition(a)) {
            collected <- c(collected, attr(a, attribute))
        }
    }
    collected
}

