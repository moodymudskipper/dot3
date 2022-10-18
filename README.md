
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dot3 <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

{dot3} providesß tools to manipulate the ellipsis object, also known as
dots (`...`).

## Installation

Install with:

``` r
devtools::install_github("moodymudskipper/dot3")
```

## Context: understanding dots

The ellipsis `...` in R is a special kind of object. It has the type
`"..."` (you can see it enumerated in the different base types in
`?typeof`) and cannot be manipulated like other objects.

``` r
# This doesn't work
f <- function(...) ...
d <- f(a=1)
#> Error in f(a = 1): '...' used in an incorrect context

# but this does
f <- function(...) get("...")
d <- f(a=1)

# printing it doesn't say much
d
#> <...>
typeof(d)
#> [1] "..."
```

Though it’s not typically done, and is hard to do without having R
complain, we can manipulate these objects

``` r
with(list(... = d), data.frame(..., b=2))
#>   a b
#> 1 1 2
```

Arguments defined through dots are defined lazily (only evaled when
needed), each of then has 3 important properties :

-   a name (optional)
-   an expression
-   an environment where the expression will be evaled when needed

{dot3} offers tools to easily manipulate dots, this includes:

-   creating them
-   subsetting them and reordering them
-   renaming their dot arguments
-   setting evaluation environments
-   combining them
-   evaluating a dot argument selectively

## Create and manipulate “dots” objects

We can create “dots” objects with the `dots()` function.

``` r
library(dot3)
dots1 <- dots(a=x, x+y)
```

The evaluation environment of these dot args will be the local
environment, if we need to set another environment we can use
`dots_in_env()`

We can use `with_dots()` to provide the dots as arguments to a function
call.

``` r
x <- 1
y <- 2
with_dots(dots1, c(...))
#> a   
#> 1 3
```

The above is really equivalent to `with(list(... = dots1), c(...))`,
just a bit more compact and efficient.

Objects created with `dots()` also have a class “dots”, that allows
printing, subsetting, combining, and naming.

``` r
dots1 <- dots(a=x, x+y)

# printing displays the argument positions, environments, names (if relevant), and expression 
dots1
#> <dots> object
#> ..1: R_GlobalEnv a = x
#> ..2: R_GlobalEnv x + y

# we can subset dots, this includes reordering them
dots1[2:1]
#> <dots> object
#> ..1: R_GlobalEnv x + y
#> ..2: R_GlobalEnv a = x

# subsetting with `[[` or `$` evaluates the item in the relevant environment, without
# triggering evaluation of other elements of the "dots" object
dots1$a
#> [1] 1
dots1[[2]]
#> [1] 3

# we can use c() to combine
dots2 <- dots(b = y)
dots3 <- c(dots1, dots2)

# we can edit the names
names(dots3)[1] <- "X"
# and the environments
dots_env(dots3[1:2]) <- asNamespace("stats")
dots3
#> <dots> object
#> ..1: namespace:stats X = x
#> ..2: namespace:stats x + y
#> ..3: R_GlobalEnv b = y
```

In practice `dots()` will mostly be used inside of functions, to
manipulate the dot arguments originally provided to the function.

``` r
f <- function(...) {
  dots1 <- dots()
  dots2 <- dots(...) # the same
  dots3 <- dots(b = y)
  dots4 <- dots(..., b = y)
  list(
    dots1 = dots1,
    dots2 = dots2,
    dots3 = dots3,
    dots4 = dots4
  )
}
f(a = x)
#> $dots1
#> <dots> object
#> ..1: R_GlobalEnv a = x
#> 
#> $dots2
#> <dots> object
#> ..1: R_GlobalEnv a = x
#> 
#> $dots3
#> <dots> object
#> ..1: 0x122526c20 b = y
#> 
#> $dots4
#> <dots> object
#> ..1: R_GlobalEnv a = x
#> ..2: 0x122526c20 b = y
```

## Convert arguments to/from dots

`endots()` creates a single argument `"dots"` object from an argument
(or any object or unevaled promise).

`enarg()` creates an unevaled promise from a one argument `"dots"`
object, *as a side effect*.

These names are inspired to the `en*()` family of functions of {rlang}
(`enquo()`, `enexpr()` etc).

``` r
f <- function(x) {
 dots1 <- endot(x)
 dots1
}
f(y+z)
#> <dots> object
#> ..1: 0x142c24e78 x = y + z

g <- function(...) {
  d <- dots(...)
  enarg(d[1]) # create promise using name of dot argument
  enarg(d[2], "z") # use provided name instead
  message("not yet evaled")
  x + z
}
g(x = 1, y = 2)
#> not yet evaled
#> [1] 3
try(g(x = 1, y = stop()))
#> not yet evaled
#> Error in g(x = 1, y = stop()) :
```

## dots_expand

This function is used to support the syntax `fun(... = alist())`, see
example below :

``` r
subset2 <- function(...) {
  dots_expand() # the only thing we need is this call before dots are used in the function
  subset(...)
}

# it still works as it would without the feature
subset2(cars, speed == 4)
#>   speed dist
#> 1     4    2
#> 2     4   10

# we can provide all dot arguments to the `... =` argument
args <- alist(cars, speed == 4)
subset2(... = args)
#>   speed dist
#> 1     4    2
#> 2     4   10

# or combine both ways
args <- alist(speed == 4)
subset2(cars, ... = args)
#>   speed dist
#> 1     4    2
#> 2     4   10
```

This way arguments can easily be provided to a function without using
`do.call()`, it can be compared to the use of `!!!` in {rlang}, except
that it will work with lazily evaled arguments too.
