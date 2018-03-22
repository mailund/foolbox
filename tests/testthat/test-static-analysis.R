context("Static analysis")

## Collection ###########################################################
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


## Annotation ###########################################################
test_that("we can annotate with the symbols in a simple function", {
    f <- function() {
        42
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), character(0))

    f <- function() {
        x <- 42
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), "x")

    f <- function() {
        x <- 42
        y <- 24
        x + y
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), c("x", "y"))

    f <- function(x = 42) {
        y <- 24
        x + y
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), "y")

    # If we analyse the full function, we might not want to consider
    # formal parameters as local variables, but inside the *body*
    # of this function, we do assign to `x`, so we include it in
    # the annotation.
    f <- function(x = 42) {
        x <- 42
        y <- 24
        x + y
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), c("x", "y"))

    # we shouldn't include duplications
    f <- function(x = 42) {
        y <- x
        x <- 42
        y <- x + 2
        x + y
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), c("y", "x"))
})

test_that("we can annotate with symbols when there are for-loops", {
    f <- function(n) {
        for (i in 1:n) i
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), "i")

    f <- function(n) {
        x <- 0
        for (i in 1:n) {
            x <- x + i
        }
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), c("x", "i"))
})

test_that("we don't annotate with symbols we know are in a different scope", {
    f <- function(df) {
        with(df, z <- x + y)
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), character())

    f <- function(x) {
        y <- 2 * x
        function(x) {
            z <- 2 * x
        }
    }
    res <- annotate_assigned_symbols_in_function(f)
    expect_equal(attr(body(res), "assigned_symbols"), c("y"))
    nested_body <- body(res)[[3]][[3]]
    expect_equal(attr(nested_body, "assigned_symbols"), c("z"))
})
