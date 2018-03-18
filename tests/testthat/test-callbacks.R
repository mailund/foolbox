context("Callbacks")

test_that("Default callbacks look like they should", {
    callbacks <- callbacks()
    expect_true(identical(callbacks$atomic, foolbox::identity_callback))
    expect_true(identical(callbacks$pairlist, foolbox::identity_callback))
    expect_true(identical(callbacks$symbol, foolbox::identity_callback))
    expect_true(identical(callbacks$primitive, foolbox::identity_callback))
    expect_true(identical(callbacks$call, foolbox::identity_callback))
})


test_that("We can set callbacks", {
    f <- function(x) {
        print("f")
        x
    }
    cb <- callbacks() %>% with_atomic_callback(f)
    expect_equal(cb$atomic, f)
    expect_equal(cb$pairlist, identity_callback)
    expect_equal(cb$symbol, identity_callback)
    expect_equal(cb$primitive, identity_callback)
    expect_equal(cb$call, identity_callback)

    cb <- callbacks() %>% with_pairlist_callback(f)
    expect_equal(cb$atomic, identity_callback)
    expect_equal(cb$pairlist, f)
    expect_equal(cb$symbol, identity_callback)
    expect_equal(cb$primitive, identity_callback)
    expect_equal(cb$call, identity_callback)

    cb <- callbacks() %>% with_symbol_callback(f)
    expect_equal(cb$atomic, identity_callback)
    expect_equal(cb$pairlist, identity_callback)
    expect_equal(cb$symbol, f)
    expect_equal(cb$primitive, identity_callback)
    expect_equal(cb$call, identity_callback)

    cb <- callbacks() %>% with_primitive_callback(f)
    expect_equal(cb$atomic, identity_callback)
    expect_equal(cb$pairlist, identity_callback)
    expect_equal(cb$symbol, identity_callback)
    expect_equal(cb$primitive, f)
    expect_equal(cb$call, identity_callback)

    cb <- callbacks() %>% with_call_callback(f)
    expect_equal(cb$atomic, identity_callback)
    expect_equal(cb$pairlist, identity_callback)
    expect_equal(cb$symbol, identity_callback)
    expect_equal(cb$primitive, identity_callback)
    expect_equal(cb$call, f)

    cb <- callbacks() %>% with_primitive_callback(f) %>% with_call_callback(f)
    expect_equal(cb$atomic, identity_callback)
    expect_equal(cb$pairlist, identity_callback)
    expect_equal(cb$symbol, identity_callback)
    expect_equal(cb$primitive, f)
    expect_equal(cb$call, f)
})
