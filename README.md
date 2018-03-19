
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Function manipulation toolbox

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--03--19-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-green.svg?style=flat-square)](commits/master)

[![Travis build
status](https://travis-ci.org/mailund/foolbox.svg?branch=master)](https://travis-ci.org/mailund/foolbox)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mailund/foolbox?branch=master&svg=true)](https://ci.appveyor.com/project/mailund/foolbox)
[![Coverage
Status](http://img.shields.io/codecov/c/github/mailund/foolbox/master.svg)](https://codecov.io/github/mailund/foobox?branch=master)
[![Coverage
Status](http://coveralls.io/repos/github/mailund/foolbox/badge.svg?branch=master)](https://coveralls.io/github/mailund/foolbox?branch=master)

The `foolbox` package implements functionality for static analysis of R
functions and for manipulating functions by rewriting the components
they consist of.

## Installation

<!--
You can install the released version of foolbox from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("foolbox")
```

And the development version from [GitHub](https://github.com/) with:
-->

``` r
# install.packages("devtools")
devtools::install_github("mailund/foolbox")
```

## Examples

The `pmatch` package implements a function for translating
`cases`-calling function into functions that use `if`-`else`-statements
instead. The full implementation looks like this:

``` r
library(pmatch)

transform_cases_call <- function(expr) {
    stopifnot(rlang::call_name(expr) == "cases")

    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}

transform_cases_function_rec <- function(expr) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        call_args <- rlang::call_args(expr)
        for (i in seq_along(call_args)) {
            expr[[i + 1]] <- transform_cases_function_rec(call_args[[i]])
        }
        if (rlang::call_name(expr) == "cases") {
            expr <- transform_cases_call(expr)
        }
        expr
    }
}

transform_cases_function <- function(fun) {
    if (!rlang::is_closure(fun)) {
        err <- simpleError("Function must be a closure to be transformed")
        stop(err)
    }
    body(fun) <- transform_cases_function_rec(body(fun))
    fun
}
```

where the `cases_expr` is a function provided by the package.

If we have a function that calls `pmatch::cases`, like this one

``` r
tree := L(val) | T(left : tree, right : tree)
is_leaf <- function(tree) cases(tree, L(v) -> TRUE, otherwise -> FALSE)
is_leaf(L(2))
#> [1] TRUE
is_leaf(T(L(1), L(2)))
#> [1] FALSE
```

we can transform it using

``` r
is_leaf_tr <- transform_cases_function(is_leaf)
is_leaf_tr
#> function (tree) 
#> if (!rlang::is_null(..match_env <- pmatch::test_pattern(tree, 
#>     L(v)))) with(..match_env, TRUE) else if (!rlang::is_null(..match_env <- pmatch::test_pattern(tree, 
#>     otherwise))) with(..match_env, FALSE)
is_leaf_tr(L(2))
#> [1] TRUE
is_leaf_tr(T(L(1), L(2)))
#> [1] FALSE
```

Using `foolbox` we can implement this simpler using a single callback
function

``` r
transform_cases_cb <- function(expr, env, param) {
    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}
transform_cases <- callbacks() %>% 
    add_call_callback(pmatch::cases, transform_cases_cb) %>% 
    make_transform_function
```

Using this function has exactly the same effect as using the longer
verison,

``` r
is_leaf_foolbox <- transform_cases(is_leaf)

body(is_leaf_tr) == body(is_leaf_foolbox)
#> [1] TRUE
```

*and* has the added benefit that the dispathing to the callback is done
on function *identity* and not *name*. With this function, we recognize
`cases` as the one in scope where we define the function–the global
scope where `cases` is currently `pmatch::cases`

``` r
leaf_sum <- function(t) {
    cases(
        t,
        L(v) -> v,
        T(left, right) -> leaf_sum(left) + leaf_sum(right)
    )
}
transform_cases(leaf_sum)
#> function (t) 
#> {
#>     if (!rlang::is_null(..match_env <- pmatch::test_pattern(t, 
#>         L(v)))) 
#>         with(..match_env, v)
#>     else if (!rlang::is_null(..match_env <- pmatch::test_pattern(t, 
#>         T(left, right)))) 
#>         with(..match_env, leaf_sum(left) + leaf_sum(right))
#> }
```

while in this function

``` r
f <- function(cases, x, y, z) cases(x, y, z)
f(ifelse, TRUE, 1, 2)
#> [1] 1
```

we recognize that `cases` is a parameter and we do not transform it:

``` r
transform_cases(f)
#> function (cases, x, y, z) 
#> cases(x, y, z)
```

Since the transformation is done before we know the concrete arguments,
this of course also means that the transformation isn’t applied even if
you later on *do* use `cases`.
