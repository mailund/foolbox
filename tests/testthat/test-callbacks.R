context("Callbacks")

test_that("Default callbacks look like they should", {
    callbacks <- rewrite_callbacks()
    expect_true(identical(
        callbacks$atomic, foolbox::identity_rewrite_callback
    ))
    expect_true(identical(
        callbacks$pairlist, foolbox::identity_rewrite_callback
    ))
    expect_true(identical(
        callbacks$symbol, foolbox::identity_rewrite_callback
    ))
    expect_true(identical(
        callbacks$primitive, foolbox::identity_rewrite_callback
    ))
    expect_true(identical(
        callbacks$call, foolbox::identity_rewrite_callback
    ))
})


test_that("We can set callbacks", {
    f <- function(x) {
        print("f")
        x
    }
    cb <- rewrite_callbacks() %>% with_atomic_callback(f)
    expect_equal(cb$atomic, f)
    expect_equal(cb$pairlist, identity_rewrite_callback)
    expect_equal(cb$symbol, identity_rewrite_callback)
    expect_equal(cb$primitive, identity_rewrite_callback)
    expect_equal(cb$call, identity_rewrite_callback)

    cb <- rewrite_callbacks() %>% with_pairlist_callback(f)
    expect_equal(cb$atomic, identity_rewrite_callback)
    expect_equal(cb$pairlist, f)
    expect_equal(cb$symbol, identity_rewrite_callback)
    expect_equal(cb$primitive, identity_rewrite_callback)
    expect_equal(cb$call, identity_rewrite_callback)

    cb <- rewrite_callbacks() %>% with_symbol_callback(f)
    expect_equal(cb$atomic, identity_rewrite_callback)
    expect_equal(cb$pairlist, identity_rewrite_callback)
    expect_equal(cb$symbol, f)
    expect_equal(cb$primitive, identity_rewrite_callback)
    expect_equal(cb$call, identity_rewrite_callback)

    cb <- rewrite_callbacks() %>% with_primitive_callback(f)
    expect_equal(cb$atomic, identity_rewrite_callback)
    expect_equal(cb$pairlist, identity_rewrite_callback)
    expect_equal(cb$symbol, identity_rewrite_callback)
    expect_equal(cb$primitive, f)
    expect_equal(cb$call, identity_rewrite_callback)

    cb <- rewrite_callbacks() %>% with_call_callback(f)
    expect_equal(cb$atomic, identity_rewrite_callback)
    expect_equal(cb$pairlist, identity_rewrite_callback)
    expect_equal(cb$symbol, identity_rewrite_callback)
    expect_equal(cb$primitive, identity_rewrite_callback)
    expect_equal(cb$call, f)

    cb <- rewrite_callbacks() %>%
        with_primitive_callback(f) %>%
        with_call_callback(f)
    expect_equal(cb$atomic, identity_rewrite_callback)
    expect_equal(cb$pairlist, identity_rewrite_callback)
    expect_equal(cb$symbol, identity_rewrite_callback)
    expect_equal(cb$primitive, f)
    expect_equal(cb$call, f)
})
