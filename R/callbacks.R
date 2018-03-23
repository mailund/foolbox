#' A callback that does not do any transformation.
#'
#' Callbacks have one required argument, `expr`, but will actually be called
#' with more. The additional named parameters are:
#'
#' - **env**      The function environment of the function we are transforming
#'
#' - **params**   The formal parameters of the function we are transforming
#'
#' - **topdown**  Data passed top-down in the traversal.
#'
#' - **bottomup** Data collected by depth-first traversals before a callback is
#' called. plus whatever the user provide to [depth_first_rewrite_function()] or
#' [depth_first_analyse_function()].
#'
#' In bottom up analyses, the [merge_bottomup()] function can be used to
#' collected the results of several recursive calls. When annotating
#' expressions, the [collect_from_args()] can be used in `call` callbacks to
#' extract annotation information from call arguments.
#'
#' @param expr     The expression to (not) transform.
#' @param bottomup Information gathered depth-first in analysis callbacks. This
#'   parameter is only passed to callbacks in analysis traversals and not
#'   rewrite traversals.
#' @param ...      Additional named parameters.
#' @return `expr`
#'
#' @seealso merge_bottomup
#' @seealso collect_from_args
#'
#' @describeIn identity_rewrite_callback Identity for expression rewriting
#' @export
identity_rewrite_callback <- function(expr, ...) expr



#' @describeIn identity_rewrite_callback Identity for expression rewriting
#' @export
identity_analysis_callback <-
    function(expr, bottomup, ...) merge_bottomup(bottomup)


#' Top-down analysis callback.
#'
#' @param expr    The expression before we modify it.
#' @param topdown Information from further up the expression tree.
#' @param skip    An escape function. If called, the transformation
#'                or analysis traversal will skip this expression
#'                and continue at the sibling level.
#' @param ...     Additional data that might be passed along
#' @return Updated `topdown` information.
#' @export
nop_topdown_callback <- function(expr, topdown, skip, ...) topdown

#' Default expression-transformation callbacks.
#'
#' Callbacks must be functions that take three arguments: The expression to
#' rewrite, the environment of the function we are rewriting (i.e. the
#' environment it is defined in, not the function call frame), and a list of
#' formal parameters of the function we are translating.
#'
#' The flow of a depth-first traversal is as follows:
#'
#' For expressions that are atomic, i.e. are either atomic values, pairlists,
#' symbols, or primitives, the corresponding callback is called with the
#' expression. The callbacks are called with the expression, `expr`, the
#' environment of the function we are traversing, `env`, the parameters of that
#' function, `params`, information collected top-down in `topdown`, warning
#' flags through the `wflags` parameter, and any additional user-provided
#' arguments through `...`. If the callbacks are used in a rewrite traversal,
#' see [depth_first_rewrite_function()], they must return an expression. This
#' expression will be inserted as a substitute of the `expr` argument in the
#' function being rewritten. If the callback is part of an analysis, see
#' [depth_first_analyse_function()], then it can return any data; what it
#' returns will be provided to the callbacks on the enclosing expression via the
#' `bottomup` parameter.
#'
#' For `call` expressions, the `topdown` callback is invoked before the call is
#' traversed. It is provided with the same arguments as the other callbacks and
#' in addition a thunk `skip` that it can use to prevent the depth-first
#' traversal to explore the call further. Whatever the `topdown` callback
#' returns will be provided to the call callback via the argument `topdown` it
#' it is called (i.e. if the `topdown` callback doesn't invoke `skip`).
#'
#' After the `topdown` callback is executed, if it doesn't call `skip`, the
#' `call` callback is called on the expression. It is called with the same
#' arguments as the other callbacks, and must return an expression if part of a
#' rewrite traversal or any collected information if part of an analysis
#' traversal.
#'
#' @param callbacks The list of callbacks
#' @param fn        A function to install as a callback.
#'
#' @seealso with_atomic_callback
#' @seealso with_pairlist_callback
#' @seealso with_symbol_callback
#' @seealso with_primitive_callback
#' @seealso with_call_callback
#' @seealso with_topdown_callback
#' @seealso warning_flags
#' @export

# I'm using a function here, although it would be more natural to just use the
# value, because somehow the function identity gets messed up in covr

#' @describeIn rewrite_callbacks Default callbacks for rewriting expressions
rewrite_callbacks <- function() list(
        atomic = identity_rewrite_callback,
        pairlist = identity_rewrite_callback,
        symbol = identity_rewrite_callback,
        primitive = identity_rewrite_callback,
        call = identity_rewrite_callback,
        topdown = nop_topdown_callback
    )

#' @describeIn rewrite_callbacks Default callbacks for analysing expressions
#' @export
analysis_callbacks <- function() list(
        atomic = identity_analysis_callback,
        pairlist = identity_analysis_callback,
        symbol = identity_analysis_callback,
        primitive = identity_analysis_callback,
        call = identity_analysis_callback,
        topdown = nop_topdown_callback
    )

#' Create a function for setting callbacks.
#'
#' @param cb_name The name of the callback to set
#' @return A function that can be used in a pipe to set a callback.
# I disable coverage here since it only tracks the closure body
# and not the actual function in the tests (the function is called)
# before the tests are run, when the package is built.
# nocov start
make_with_callback <- function(cb_name) {
    force(cb_name)
    function(callbacks, fn) {
        callbacks[[cb_name]] <- fn
        callbacks
    }
}
# nocov end

#' @describeIn rewrite_callbacks Set the atomic callback function.
#' @export
with_atomic_callback <- make_with_callback("atomic")
#' @describeIn rewrite_callbacks Set the pairlist callback function.
#' @export
with_pairlist_callback <- make_with_callback("pairlist")
#' @describeIn rewrite_callbacks Set the symbol callback function.
#' @export
with_symbol_callback <- make_with_callback("symbol")
#' @describeIn rewrite_callbacks Set the primitive callback function.
#' @export
with_primitive_callback <- make_with_callback("primitive")
#' @describeIn rewrite_callbacks Set the call callback function.
#' @export
with_call_callback <- make_with_callback("call")
#' @describeIn rewrite_callbacks Set the topdown information passing callback
#'   function.
#' @export
with_topdown_callback <- make_with_callback("topdown")

#' Add a function-specific callback to the call callbacks.
#'
#' This function adds to the existing call callback, rather than replace it,
#' by putting a callback in front of it to be tested first. The callback will
#' be invoked when the traversal sees a call to a specific function.
#'
#' The callback that is installed will be called with the usual callback
#' arguments (which depend on context and user-provided information to
#' ..., see [rewrite_callbacks()] and [analysis_callbacks()]), and additionally
#' the next callback in line, through the parameter `next_cb`. This can be
#' used to propagate information through several callbacks in a pipe-like
#' fashion.
#'
#' @param callbacks The existing callbacks.
#' @param fn        The function to which calls should be modified.
#' @param cb        The callback function to invoke.
#'
#' @return          The updated callbacks.
#' @export
add_call_callback <- function(callbacks, fn, cb) {
    next_cb <- callbacks$call

    fn_expr <- rlang::enexpr(fn)
    fn_name <- if (rlang::is_symbol(fn_expr)) as.character(fn_expr) else ""

    force(fn)
    force(cb)

    closure <- function(expr, env, params, wflags, ...) {
        # make sure the call is not to a local variable--if it is,
        # we can't evaluate it at transformation time. We propagate
        # to the next callback.
        call_name <- as.character(expr[[1]])
        if (call_name %in% names(params)) {
            return(next_cb(
                expr,
                env = env, params = params, wflags = wflags, ...
            ))
        }
        # The same goes for other bound variables, if we have annotated
        # the expressions with those.
        if (call_name %in% attr(expr, "bound")) {
            if (fn_name == call_name && wflags$warn_on_local_function) {
                warning(paste0(
                    "The function ", call_name,
                    " is not processed by a callback because it might",
                    " be referring to a local variable in the scope where",
                    "it is found.\n",
                    "Use a fully qualified name if you want it processed."
                ))
            }
            return(next_cb(
                expr,
                env = env, params = params, wflags = wflags, ...
            ))
        }

        # now try to get the actual function by evaluating it
        err_fun <- function(e) {
            if (wflags$warn_on_unknown_function) {
                  warning(paste0(
                      "The function ", call_name,
                      " could not be evaluated to an actual function in ",
                      "this scope."
                  ))
              }
            NULL
        }
        fun <- tryCatch(eval(expr[[1]], env), error = err_fun)
        if (!is.null(fun) && identical(fun, fn)) {
            return(cb(
                expr,
                env = env, params = params,
                next_cb = next_cb, wflags = wflags, ...
            ))
        } else {
            # default for closure: try the next in line
            next_cb(
                expr,
                env = env, params = params, wflags = wflags, ...
            )
        }
    }
    callbacks$call <- closure
    callbacks
}

#' Add a function-specific callback to the top-down callbacks.
#'
#' This function adds to the existing topdown callback, rather than replace it,
#' by putting a callback in front of it to be tested first. The callback will
#' be invoked when the traversal sees a call to a specific function.
#'
#' The callback that is installed will be called with the usual callback
#' arguments (which depend on context and user-provided information to
#' ..., see [rewrite_callbacks()] and [analysis_callbacks()]), and additionally
#' the next callback in line, through the parameter `next_cb`. This can be
#' used to propagate information through several callbacks in a pipe-like
#' fashion.
#'
#'
#' @param callbacks The existing callbacks.
#' @param fn        The function to which calls should be modified.
#' @param cb        The callback function to invoke.
#'
#' @return          The updated callbacks.
#' @export
add_topdown_callback <- function(callbacks, fn, cb) {
    next_cb <- callbacks$topdown
    fn_expr <- rlang::enexpr(fn)
    fn_name <- if (rlang::is_symbol(fn_expr)) as.character(fn_expr) else ""

    force(fn)
    force(cb)

    closure <- function(expr, env, params, wflags, ...) {
        # make sure the call is not to a local variable--if it is,
        # we can't evaluate it at transformation time. We propagate
        # to the next callback.
        call_name <- as.character(expr[[1]])
        if (call_name %in% names(params)) {
            return(next_cb(
                expr,
                env = env, params = params, wflags = wflags, ...
            ))
        }
        # The same goes for other bound variables, if we have annotated
        # the expressions with those.
        if (call_name %in% attr(expr, "bound")) {
            if (fn_name == call_name && wflags$warn_on_local_function) {
                warning(paste0(
                    "The function ", call_name,
                    " is not processed by a callback because it might",
                    " be referring to a local variable in the scope where",
                    "it is found.\n",
                    "Use a fully qualified name if you want it processed."
                ))
            }
            return(next_cb(
                expr,
                env = env, params = params, wflags = wflags, ...
            ))
        }

        # now try to get the actual function by evaluating it
        err_fun <- function(e) {
            if (wflags$warn_on_unknown_function) {
                  warning(paste0(
                      "The function ", call_name,
                      " could not be evaluated to an actual function in ",
                      "this scope."
                  ))
              }
            NULL
        }
        fun <- tryCatch(eval(expr[[1]], env), error = err_fun)
        if (!is.null(fun) && identical(fun, fn)) {
            return(cb(
                expr,
                env = env, params = params,
                next_cb = next_cb, wflags = wflags, ...
            ))
        } else {
            # default for closure: try the next in line
            next_cb(
                expr,
                env = env, params = params,
                wflags = wflags, ...
            )
        }
    }
    callbacks$topdown <- closure
    callbacks
}
