context("Depth first transformation")

test_that("we can do simple transformations", {
    calls <- c() # I put this here so lintr can see it, but
                 # it is actually in the *transformed* function's
                 # scope it will be found and not the transformator's.
                 # In this test it is the same, though.

    log_calls_transform <- function(call_expr, env, params) {
        call_fn <- eval(call_expr[[1]], envir = env)
        if (rlang::is_primitive(call_fn)) return(call_expr)
        if (!rlang::is_symbol(call_expr[[1]])) return(call_expr)
        # call is to a named function
        rlang::expr({
            calls <<- c(rlang::UQ(as.character(call_expr[[1]])), calls)
            rlang::UQ(call_expr)
        })
    }
    cb <- callbacks %>% with_call_callback(log_calls_transform)

    f <- function(x) {
        if (x > 0) f(x - 1)
    }

    f <- depth_first_rewrite_function(f, cb)
    f(3)
    expect_equal(calls, c("f", "f", "f"))
})
