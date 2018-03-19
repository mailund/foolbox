context("Depth first transformation")

test_that("we can do simple transformations", {
    calls <- c() # I put this here so lintr can see it, but
    # it is actually in the *transformed* function's
    # scope it will be found and not the transformator's.
    # In this test it is the same, though.

    log_calls_transform <- function(call_expr, env, ...) {
        call_fn <- eval(call_expr[[1]], envir = env)
        if (rlang::is_primitive(call_fn)) return(call_expr)
        if (!rlang::is_symbol(call_expr[[1]])) return(call_expr)
        # call is to a named function
        rlang::expr({
            calls <<- c(rlang::UQ(as.character(call_expr[[1]])), calls)
            rlang::UQ(call_expr)
        })
    }
    cb <- callbacks() %>% with_call_callback(log_calls_transform)

    f <- function(x) {
        if (x > 0) f(x - 1)
    }

    f <- depth_first_rewrite_function(f, cb)
    f(3)
    expect_equal(calls, c("f", "f", "f"))
})

test_that("we call callback on pairlist", {
    pairlist_called <- FALSE
    pairlist_cb <- function(call_expr, ...) {
        pairlist_called <<- TRUE
        call_expr
    }
    f <- function() function(x, y) x + y
    expect_equal(f()(2, 2), 4)

    expect_false(pairlist_called)
    cb <- callbacks() %>% with_pairlist_callback(pairlist_cb)
    g <- depth_first_rewrite_function(f, cb)
    expect_true(pairlist_called)
    expect_equal(g()(2, 2), 4)
})

test_that("we call callback on primitive", {
    # it is actually pretty hard to see a "primitive" when
    # traversing quoted expressions, so this is the only
    # way I could think of to test this
    primitive_called <- FALSE
    primitive_cb <- function(expr, ...) {
        primitive_called <<- TRUE
        expr
    }
    cb <- callbacks() %>% with_primitive_callback(primitive_cb)
    depth_first_rewrite_expr(`if`, cb, environment(), list())
    expect_true(primitive_called)
})

test_that("we can call a callback for a specific function", {
    f <- function(x) x
    g <- function(y) y + f(y)
    f_cb <- function(expr, env, ...) {
        call_fn <- eval(expr[[1]], env)
        stopifnot(identical(call_fn, f))
        quote(2 + x)
    }

    cb <- callbacks() %>% add_call_callback(fn = f, cb = f_cb)
    g_tr <- depth_first_rewrite_function(g, cb)
    expect_equal(body(g_tr), quote(y + (2 + x)))

    h <- function(z) 3 * z
    g <- function(y) h(y + f(y))
    g_tr <- depth_first_rewrite_function(g, cb)
    expect_equal(body(g_tr), quote(h(y + (2 + x))))

    h_cb <- function(expr, ...) {
        rlang::expr(3 * rlang::UQ(expr[[2]]))
    }
    cb <- cb %>% add_call_callback(fn = h, h_cb)
    g_tr <- depth_first_rewrite_function(g, cb)
    expect_equal(body(g_tr), quote(3 * (y + (2 + x))))
})

test_that("we can handle call-callbacks when there are local functions", {
    f <- function(x) x

    f_cb <- function(expr, env, ...) {
        call_fn <- eval(expr[[1]], env)
        stopifnot(identical(call_fn, f))
        quote(2 + x)
    }
    cb <- callbacks() %>% add_call_callback(fn = f, cb = f_cb)

    g <- function(y) y + f(y)
    g_tr <- depth_first_rewrite_function(g, cb)
    expect_equal(body(g_tr), quote(y + (2 + x)))

    h <- function(f, y) y + f(y)
    h_tr <- depth_first_rewrite_function(h, cb)
    expect_equal(body(h_tr), quote(y + f(y)))
})

test_that("we warn when we see unknown functions", {
    f <- function(x) x
    f_cb <- function(expr, env, ...) {
        call_fn <- eval(expr[[1]], env)
        stopifnot(identical(call_fn, f))
        quote(2 + x)
    }
    cb <- callbacks() %>% add_call_callback(fn = f, cb = f_cb)

    g <- function(y) h(y) + f(y)
    g_tr <- expect_warning(
        depth_first_rewrite_function(g, cb),
        "The function h .*"
    )
    expect_equal(body(g_tr), quote(h(y) + (2 + x)))
})

test_that("we can pass user-data along in traversals", {
    f <- function(x) x

    f_cb <- function(expr, env, n, ...) {
        rlang::expr(!!n + x)
    }
    cb <- callbacks() %>% add_call_callback(fn = f, cb = f_cb)

    g <- function(y) y + f(y)
    g_tr <- depth_first_rewrite_function(g, cb, n = 2)
    expect_equal(body(g_tr), quote(y + (2 + x)))
})
