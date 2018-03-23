context("API/DSL for specifying transformation functions")

test_that("we can call a callback for a specific function", {
    f <- function(x) 2 + x
    cb <- rewrite_callbacks() %>%
        add_call_callback(f, function(expr, ...) {
            quote(2 + x)
        })
    tr_f <- . %>% rewrite() %>% rewrite_with(cb)

    g <- function(y) y + f(y)
    expect_equal(body(tr_f(g)), quote(y + (2 + x)))

    # Here, f is a parameter, so shouldn't be transformed
    g <- function(f, y) y + f(y)
    expect_equal(body(tr_f(g)), quote(y + f(y)))

    # Here, f is a local variable, so shouldn't be transformed
    g <- function(f, y) {
        f <- function(x) 2 * x
        y + f(y)
    }
    expect_equal(body(tr_f(g)), quote({f <- function(x) 2 * x ; y + f(y)}))
})

