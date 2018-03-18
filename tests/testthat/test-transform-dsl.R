context("API/DSL for specifying transformation functions")

test_that("we can call a callback for a specific function", {
    f <- function(x) 2 + x
    tr <- callbacks() %>%
          add_call_callback(f, function(expr, env, param) {
              quote(2 + x)
          }) %>% make_transform_function

    expect_equal(body(tr(function(y) y + f(y))), quote(y + (2 + x)))

    g <- function(y) y + f(y)
    expect_equal(body(tr(g)), quote(y + (2 + x)))

    h <- function(z) 3 * z
    g <- function(y) h(y + f(y))
    expect_equal(body(tr(g)), quote(h(y + (2 + x))))

    tr <- callbacks() %>%
        add_call_callback(f, function(expr, env, param) {
            quote(2 + x)
        }) %>% add_call_callback(h, function(expr, env, params) {
            rlang::expr(3 * rlang::UQ(expr[[2]]))
        }) %>% make_transform_function
    expect_equal(body(tr(g)), quote(3 * (y + (2 + x))))
})

test_that("we can handle call-callbacks when there are local functions", {
    f <- function(x) x
    tr <- callbacks() %>%
        add_call_callback(f, function(expr, env, param) {
            quote(2 + x)
        }) %>% make_transform_function

    g <- function(y) y + f(y)
    expect_equal(body(tr(g)), quote(y + (2 + x)))

    h <- function(f, y) y + f(y)
    expect_equal(body(tr(h)), quote(y + f(y)))
})
