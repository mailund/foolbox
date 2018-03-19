#' Create a transformation function from a callback specification.
#'
#' @param callbacks The callbacks to apply to transformed functions.
#' @return A function that can transform other functions.
#' @export
make_transform_function <- function(callbacks) {
    force(callbacks)
    function(fun, ...) depth_first_rewrite_function(fun, callbacks, ...)
}
