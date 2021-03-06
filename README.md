
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Function manipulation toolbox

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--12--20-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.1.9000-green.svg?style=flat-square)](commits/master)
[![Travis build
status](https://travis-ci.org/mailund/foolbox.svg?branch=master)](https://travis-ci.org/mailund/foolbox)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mailund/foolbox?branch=master&svg=true)](https://ci.appveyor.com/project/mailund/foolbox)
[![Coverage
Status](http://img.shields.io/codecov/c/github/mailund/foolbox/master.svg)](https://codecov.io/github/mailund/foobox?branch=master)
[![Coverage
Status](http://coveralls.io/repos/github/mailund/foolbox/badge.svg?branch=master)](https://coveralls.io/github/mailund/foolbox?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/foolbox)](https://cran.r-project.org/package=foolbox)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/foolbox)](https://cran.r-project.org/package=foolbox)
[![minimal R
version](https://img.shields.io/badge/R-%E2%89%A53.2-blue.svg)](https://cran.r-project.org/)

The `foolbox` package implements functionality for static analysis of R
functions and for manipulating functions by rewriting the components
they consist of. The package was written to collect similar
functionality from the [pmatch](https://github.com/mailund/pmatch) and
[tailr](https://github.com/mailund/tailr) packages, that both have
functions for rewriting other functions, but is a general framework for
static analysis and function rewriting.

The functionality centres on depth-first traversals of expression trees,
typically the body of functions. For example, if you have the function
`f`

``` r
f <- function(x) {
  y <- 2 * x
  x + y
}
```

then its body is an expression, a `call` to the function `{` with
arguments `y <- 2 * x` and `x + y`:

``` r
expr <- body(f)
expr[[1]]
#> `{`
expr[[2]]
#> y <- 2 * x
expr[[3]]
#> x + y
```

The first statement inside `f`’s body is another `call`, this time to
the `<-` function, and this call takes two arguments, the `symbol` `y`
and the expression `2 * x` which is yet another call, to `*`, with the
`atomic` 2 and `symbol` `x` as arguments.

With `foolbox` you can travers such expression structures and rewrite
them based on callbacks. You can define callbacks for four base cases
for expressions, `atomic`, `pairlist`, `symbol` and `primitive`, for the
recurse `call` expressions and a callback, called `topdown`, invoked
before the traversal recurses into a `call` object.

You specify how you want to transform an expression by composing a set
of callbacks for a transformation and you apply several transformations
by specifying a pipeline of these.

Say, for example, you have functions

``` r
f <- function(x) 2 * x
g <- function(y) f(y)
```

and you want to replace the function call `f(y)` in the body of `g` with
the function body, `2 * x`. You can do this by installing a callback for
calls to `f` and then rewrite with this:

``` r
callbacks <- rewrite_callbacks() %>% 
    add_call_callback(f, function(expr, ...) quote(2 * x))

g %>% rewrite() %>% rewrite_with(callbacks)
#> function (y) 
#> 2 * x
```

Here, I’ve constructed the callbacks first, but a more natural approach
might be to provide them inside the pipeline like this:

``` r
g %>% rewrite() %>% rewrite_with(
    rewrite_callbacks() %>% add_call_callback(f, function(expr, ...) quote(2 * x))
)
#> function (y) 
#> 2 * x
```

At least, that is what I find myself doing as I am experimenting with
`foolbox`.

If you have transformations you apply on more than one function, you can
of course save them

``` r
subst_f <- . %>% rewrite() %>% rewrite_with(
    rewrite_callbacks() %>% add_call_callback(f, function(expr, ...) quote(2 * x))
)
```

and apply them later

``` r
g %>% subst_f
#> function (y) 
#> 2 * x
```

If you have such saved transformations you can also use them as part of
function definition

``` r
h <- rewrites[subst_f] < function(x) f(x) + 2 * f(x)
h
#> function (x) 
#> 2 * x + 2 * (2 * x)
```

You can also put the full definition of a transformation in this syntax,
but it is less readable.

The documentation is currently a bit sparse. All functions are
documented, but I haven’t written documentation for the overall design.
That is on its way. For now, check the examples below.

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

Below are a few examples of things I thought up after writing the
`foolbox`. Serendipitous discoveries that I didn’t design the package
for, but that are easy to implement using it. I haven’t taken the ideas
very far — it is possible to do much more with them, but that would make
the exampels harder to follow.

### Invariants

Say you want to add an invariant to a variable in a function. Whenever
you assign to that variable, you want to make sure the invariant is
`TRUE`. We can insert invariant checking into an existing function using
`foolbox`.

(In this example, I do not include a test at the beginning of function,
which I probably should, since that requires that I check if the
variable is an argument or not — with `foolbox` you can easily do this,
but I keep it simple).

What you want to do is install an invariant that is called on all
assignments, i.e. `call`s to `<-` or `=`. That callback checks if the
assignment is to the variable of interest, and if it is, it replaces the
assignment with an assignment and a check.

We can write the following function for this. It expects `var` to be a
symbol and `predicate` to be an expression. It creates a callback for
assignments, and it then rewrites using that.

``` r
set_invariant <- function(fn, var, predicate) {
    
    var <- rlang::enexpr(var)
    stopifnot(rlang::is_symbol(var))
    
    predicate <- rlang::enexpr(predicate)
    
    set_invariant_callback <- function(expr, ...) {
        if (expr[[2]] == var) {
            rlang::expr({
                !!var <- !!expr[[3]]
                stopifnot(!!predicate)
            })
        } else {
            expr    
        }
    }
    
    fn %>% rewrite() %>% rewrite_with(
        rewrite_callbacks() %>%
        add_call_callback(`<-`, set_invariant_callback) %>%
        add_call_callback(`=`, set_invariant_callback)
    )
}
```

As an example, we can require that `a` is always positive in the
function below.

``` r
f <- function(x, y) {
    a <- x + y
    2 * a^2 + a
}

f %>% set_invariant(a, a > 0)
#> function (x, y) 
#> {
#>     {
#>         a <- x + y
#>         stopifnot(a > 0)
#>     }
#>     2 * a^2 + a
#> }
```

However, what happens if we have nested functions?

``` r
f <- function(x, y) {
    a <- x + y
    g <- function(a) {
        a <- a + 42
        a
    }
    h <- function(x) {
        a <<- -x
        a^2
    }
    x <- h(y)
    2 * a^2 + g(-x)
}

f %>% set_invariant(a, a > 0)
#> function (x, y) 
#> {
#>     {
#>         a <- x + y
#>         stopifnot(a > 0)
#>     }
#>     g <- function(a) {
#>         {
#>             a <- a + 42
#>             stopifnot(a > 0)
#>         }
#>         a
#>     }
#>     h <- function(x) {
#>         a <<- -x
#>         a^2
#>     }
#>     x <- h(y)
#>     2 * a^2 + g(-x)
#> }
```

Here, we probably don’t want to add the invariant inside the nested
functions. An assignment there isn’t an assignment to the variable in
the scope of `f`, after all. But we *do* want to capture `<<-`
assignments.

(For the `<<-` assignment, it is a bit tricky to see if it is to the `a`
in the scope of `f`, in general. It depends on whether that has been
assigned to in `f` before we call the nested function, and in the full
generality of functions, we cannot determine this before we run `f`. I
am going to assume that all `<<-` assignments are to the scope of `f`
for the rest of this example).

We can add `<<-` as a function to call our callback on, and we can use a
`topdown` callback to pass information on whether we are in a nested
function down the recursion.

``` r
set_invariant <- function(fn, var, predicate) {
    
    var <- rlang::enexpr(var)
    stopifnot(rlang::is_symbol(var))
    
    predicate <- rlang::enexpr(predicate)
    
    set_invariant_callback <- function(expr, topdown, ...) {
        if (expr[[2]] == var) {
            if ( (expr[[1]] == "<-" || expr[[1]] == "=") &&  !(topdown$nested) ) {
                return(rlang::expr({
                    !!var <- !!expr[[3]]
                    stopifnot(!!predicate)
                }))
            }
            if (expr[[1]] == '<<-') {
                return(rlang::expr({
                    !!var <<- !!expr[[3]]
                    stopifnot(!!predicate)
                }))
            }
        }
        # if we don't return earlier, we keep the expression
        expr    
    }
    nested_functions_callback <- function(expr, skip, topdown, ...) {
        topdown$nested <- TRUE
        topdown
    }
    
    fn %>% rewrite() %>% rewrite_with(
        rewrite_callbacks() %>%
        add_call_callback(`<-`, set_invariant_callback) %>%
        add_call_callback(`=`, set_invariant_callback) %>%
        add_call_callback(`<<-`, set_invariant_callback) %>%
        add_topdown_callback(`function`, nested_functions_callback),
        topdown = list(nested=FALSE)
    )
}
```

``` r
f <- function(x, y) {
    a <- x + y
    g <- function(a) {
        a <- a + 42
        a
    }
    h <- function(x) {
        a <<- -x
        a^2
    }
    x <- h(y)
    2 * a^2 + g(-x)
}

f %>% set_invariant(a, a > 0)
#> function (x, y) 
#> {
#>     {
#>         a <- x + y
#>         stopifnot(a > 0)
#>     }
#>     g <- function(a) {
#>         a <- a + 42
#>         a
#>     }
#>     h <- function(x) {
#>         {
#>             a <<- -x
#>             stopifnot(a > 0)
#>         }
#>         a^2
#>     }
#>     x <- h(y)
#>     2 * a^2 + g(-x)
#> }
```

If you want, you can add more than one invariant. The result nests some
code-blocks, and it might not look pretty (you can clean it up using
`foolbox` if you want), but it works.

``` r
f <- function(x, y) {
    a <- x + y
    2 * a^2 + a
}

f %>% set_invariant(a, a > 0) %>% set_invariant(a, is.numeric(a))
#> function (x, y) 
#> {
#>     {
#>         {
#>             a <- x + y
#>             stopifnot(is.numeric(a))
#>         }
#>         stopifnot(a > 0)
#>     }
#>     2 * a^2 + a
#> }
```

### Inline

As another example, imagine that you want to inline a function call.

I put some restrictions on the function in this example. I don’t allow
it to have assignments. This is for the same reasons that we had to make
assumptions about the `<<-` assignment above. We cannot, before we call
the function, know what local variables will be set to. If we inline a
function, we need to replace variables with values, and that gets tricky
if there are assignments. For similar reasons, I will assume that
default arguments can be computed from parameters that are explicitly
provided. Lazy evaluation and the rules for when variables are
evaluated, and in which context, makes it hard to know the values of
these if I cannot assume this, and the extra analysis needed is too much
for this example.

Anyway, with a simple mapping from a function call to its body, with
variables replaced by the parameters in the call, can be implemented
using two transformations. One, that replaces variables in the function
body with the values they should get in the call, and another that then
inserts this transformed body in the place where we have the function
call.

I first check that the function, `f`, doesn’t have assignments. I don’t
know how to check the requirements on the default arguments, but I
should probably also check that.

After that, I define a function that maps a symbol to a value, if the
symbol is found in a table, `map`. Then I use that in a function that
rewrites an expression; it use the mapping function as a callback on
symbols and rewrites an expression.

I then define a callback to inline function calls. This will be called
on `call` objects for the given function. I extract the arguments
provided in the call and put them in a table. Then I substitute the
values in the table into the variables in the default parameters and
rewrite the expressions there, substituting the variables I now have
there with the values they get in the function call. Finally, I take the
function body, rewrite it using the symbol map, and return that. This
will then be inserted as a replacement for the function call in the
depth-first traversal we set up as the final expression in the function.

``` r
inline <- function(fn, f) {
    
    # only inline if `f` has no assignments... otherwise, too much
    # analysis is needed for this simple example.
    check_assignment <- function(expr, ...) {
        stop("Assignment! I told you not to!")
    }
    f %>% analyse() %>% analyse_with(
        analysis_callbacks() %>%
            add_topdown_callback(`<-`, check_assignment) %>%
            add_topdown_callback(`=`, check_assignment)
    )
    
    defaults <- formals(f)
    remap_symbols <- function(expr, map, ...) {
        var_name <- as.character(expr)
        if (var_name %in% names(map)) {
            rlang::expr( ( !!map[[var_name]] ) )
        } else {
            expr
        }
    }
    remap_expr <- function(expr, map) {
        expr %>% rewrite_expr() %>% rewrite_expr_with(
            rewrite_callbacks() %>% with_symbol_callback(remap_symbols),
            map = map
        )
    }
    
    inline_callback <- function(expr, ...) {
        map <- defaults
        args <- as.list(match.call(f, expr)[-1])
        vars <- names(args)
        for (var in vars) {
            map[[var]] <- args[[var]]
        }
        for (var in names(defaults)) {
            if (! var %in% vars) {
                expr_with_varsubst <- eval(substitute(defaults[[var]], map)) 
                map[[var]] <- expr_with_varsubst %>% remap_expr(map)
            }
        }
        
        body(f) %>% remap_expr(map)
    }
    
    fn %>% rewrite() %>% rewrite_with(
        rewrite_callbacks() %>% 
            add_call_callback(f, inline_callback)
    )
}
```

To see it in action, we can inline the calls to `f` in the function `g`:

``` r
f <- function(x, y = x) 2 * x + y
g <- function(z) f(z - 3) + f(y = z + 3, x = 4)
g %>% inline(f)
#> function (z) 
#> 2 * (z - 3) + (z - 3) + (2 * 4 + (z + 3))
```

If we want to perform the inline-transformation while we define a new
function we can use the `rewrites` syntax, but here we need to change
the inline transformation function a bit. The transformations given to
`rewrites` must take the function to be transformed as its first
argument, so we need to “curry” the inline function, changing it from
`function(f, fn) ...` to `function(f) function(fn) ...` so we can
provide the function to inline in `rewrites` and get the function to
transform when the `rewrites` rules are run.

``` r
g <- rewrites[inline(f)] < function(z) f(z - 3) + f(y = z + 3, x = 4)
g
#> function (z) 
#> 2 * (z - 3) + (z - 3) + (2 * 4 + (z + 3))
```

We can write `inline(f)` here, although the `inline` function takes two
arguments, the first of which being the function we transform, because
the `rewrites[...]` syntax transform the functions in the same was as
the `%>%` operator. The input to the `rewrites[...]` is automatically
added as the first argument to the first transformation in `rewrites`,
the output of the first transformation will be inserted as the first
argument to the next transformation, etc.
