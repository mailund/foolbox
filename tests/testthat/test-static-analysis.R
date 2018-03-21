context("Static analysis")

test_that("we can collect the symbols in a simple expression", {
    expr <- quote(42)
    res <- collect_assigned_symbols_in_expression(expr, env = environment())
    expect_equal(res, list())

    expr <- quote(x <- 42)
    res <- collect_assigned_symbols_in_expression(expr, env = environment())
    expect_equal(res, list(locals = "x"))
})

test_that("we can collect the symbols in a simple function", {
    f <- function() {
        42
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, character(0))

    f <- function() {
        x <- 42
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, "x")

    f <- function() {
        x <- 42
        y <- 24
        x + y
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, c("x", "y"))

    f <- function(x = 42) {
        y <- 24
        x + y
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, "y")

    # we shouldn't include formal arguments
    f <- function(x = 42) {
        x <- 42
        y <- 24
        x + y
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, "y")

    # we shouldn't include duplications
    f <- function(x = 42) {
        y <- x
        x <- 42
        y <- x + 2
        x + y
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, "y")
})

test_that("we can collect the symbols when there are for-loops", {
    f <- function(n) {
        for (i in 1:n) i
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, "i")

    f <- function(n) {
        x <- 0
        for (i in 1:n) {
            x <- x + i
        }
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, c("x", "i"))
})

test_that("we don't collect symbols we know are in a different scope", {
    f <- function(df) {
        with(df, z <- x + y)
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, character())

    f <- function(x) {
        y <- 2 * x
        function(x) {
            z <- 2 * x
        }
    }
    res <- collect_assigned_symbols_in_function(f)
    expect_equal(res, c("y"))
})
