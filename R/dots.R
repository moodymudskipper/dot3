
is_ellipsis <- function(x) typeof(x) == "..."


#' Get or Set the Environment of a 'dots' Object
#'
#' @param x a 'dots' object
#' @param value an environment
#'
#' @export
dots_env <- function(x) {
  if (length(x) != 1) abort(c(
    "`x` must be of length 1.",
    x = sprintf("It has length %s.", length(x))
  ))
  quo_get_env(dots_to_quos(x)[[1]])
}

#' @export
#' @rdname dots_env
`dots_env<-` <- function(x, value) {
  if (!is.environment(value)) {
    abort(c(
      "`value` must be of type 'environment'",
      x = sprintf("`value` is of type '%s'", typeof(value))
    ))
  }
  x <- dots_to_quos(x)
  for (i in seq_along(x)) {
    environment(x[[i]]) <- value
  }
  as_dots(x)
}

#' Define 'dots' Object
#'
#' The output is
#' of type "...", just like the output of `get("...")`, but we added a class "dots"
#' so we could define methods for it. Note that dots are lazy,
#' their expression is not evaluated at the time of definition.
#'
#' @param ... arguments to gather, can be empty, can contain `...`
#' @param *env* environment in which to evaluate the dots
#'
#' @export
#' @examples
#' dots1 <- dots(a=x, b=y)
#' dots1
#'
#' # subset and combine as you would with a vector
#' dots1[1]
#' dots1[2:1]
#' dots1["a"]
#' dots1[c(FALSE,TRUE)]
#'
#' # `[[` and `$` evaluate the argument
#' x <- 2
#' dots1[["a"]]
#'
#' dots2 <- dots(c = i + j)
#' c(dots1, dots2)
#'
#' f <- function(...) {
#'   dots3 <- dots(...) # the same
#'   dots4 <- dots(b = y)
#'   dots5 <- dots(..., b = y)
#'   list(
#'     dots3 = dots2,
#'     dots4 = dots3,
#'     dots5 = dots4
#'   )
#' }
#' f(a = x)
#'
dots <- function(...) {
  d <- if (missing(...)) get("...", parent.frame()) else environment()$...
  class(d) <- "dots"
  d
}

#' @export
#' @rdname dots
dots_in_env <- function(`*env*`, ...) {
  if(identical(`*env*`, emptyenv())) {
    # we cannot evaluate anything in this environment since we cannot place anything there
    # we make sure they're evaled and we return them, this should use the `emptyenv()`
    list(...)
    return(environment()[["..."]])
  }
  eval(substitute(dots(...)), `*env*`)
}

#' @export
print.dots <- function(x, ...) {
  dots_enquos <- evalq(enquos(...), list(... = x))
  exprs <- vapply(dots_enquos, function(x) deparse1(quo_get_expr(x)), character(1))
  envs <- vapply(
    dots_enquos,
    function(x) {
      env_chr <- capture.output(quo_get_env(x))
      sub("<environment: (.*)>", "\\1", env_chr)
    },
    FUN.VALUE = character(1)
  )
  dot_nms <- names(x)
  if(is.null(dot_nms)) dot_nms <- rep("", length(x))
  exprs <- ifelse(dot_nms == "", exprs, paste(names(x),"=", exprs))
  out <- paste0("..", seq_along(exprs), ": ", envs, " ", exprs)
  writeLines("<dots> object")
  writeLines(out)
  invisible(x)
}


# the rlang version

#' split a "dots" object into single argument "dots"
#'
#' @param x A "dots" object built with `dots()`
#' @param ... ignored
#'
#' @return A list of single argument "dots" objects
#' @export
#'
#' @examples
#' f <- function(...) {
#'   dots1 <- dots()
#'   dot_split(dots1)
#' }
#' f(a = x, y)
dot_split <- function(x, ...) {
  dots_enquos <- evalq(enquos(...), list(... = x))
  fun <- function(x,nm) {
    arg <- list(quo_get_expr(x))
    names(arg) <- nm
    eval(as.call(c(quote(dots_in_env), quote(quo_get_env(x)),  arg)))
  }
  split_dots <- Map(fun, dots_enquos, names(dots_enquos))
  split_dots
}


#' transform an argument into a one argument "dots" object
#'
#' @param x an argument
#' @export
#' @examples
#' f <- function(x) {
#'   dots1 <- endot(x)
#'   dots1
#' }
#' f(y+z)
endot <- function(x) {
  var <- substitute(x)
  nm <- as.character(var)
  x <- eval.parent(as.call(c(quote(rlang::enquo), var)))
  arg <- list(quo_get_expr(x))
  names(arg) <- nm
  eval(as.call(c(quote(dots), arg)), list(dots=dots), quo_get_env(x))
}

#' transform a one argument "dots" object into an argument
#'
#' @param x a one argument "dots" object
#' @param nm an optional variable name, if absent the name provided to the dots
#'   will be used
#' @param ... unused
#' @export
#' @examples
#' f <- function(...) {
#'   d <- dots(...)
#'   enarg(d)
#'   enarg(d, "y")
#'   message("not yet evaled")
#'   x + y
#' }
#' f(x = 1)
#' try(f(x = stop()))
enarg <- function(x, nm = NULL, ...) {
  if(is.null(nm)) nm <- names(x)
  x <- with_dots(x, enquos(...))[[1]]
  pf <- parent.frame()
  eval(substitute(delayedAssign(
    x = nm,
    value = EXPR,
    eval.env = quo_get_env(x),
    assign.env = pf
  ), list(EXPR= quo_get_expr(x))))
}

#' @export
c.dots <- function(...) {
  # R doesn't like list(...) or c(...) with the dots containing dots object
  # so we work around to build a list of <dots> objects
  call <- sys.call()
  call[[1]] <- quote(`list`)
  dot_list <- eval.parent(call)
  env <- environment()

  # we create a call `dots(..., ..., ...)` where the `...` take in turn the values
  # of provided dots, to do this we use an active binding so that `...` updates its value every time
  i <- 0
  # free the binding
  rm("...")
  makeActiveBinding("...", fun = function() {
    i <<- i + 1
    dot_list[[i]]
  }, env = env)
  dots_call <- as.call(c(quote(dots), replicate(length(dot_list), quote(`...`))))
  eval(dots_call)
}

#' Support flexible dots in function
#'
#' @export
dots_expand <- function() {
  exec_env <- parent.frame()
  caller_env <- parent.frame(2)
  # capture dots in exec env
  old_dots <- eval(substitute(alist(...), exec_env))
  # eval "...=" arg in caller env (it it's an alist() it doesn't matter where it's evaled)
  new_dots <- eval(old_dots$..., caller_env)
  # build a function that will capture the `...=` arg in f1() since we need to remove it
  dot_editor <- as.function(c(alist(...=, ... =NULL), quote(environment()$...)))
  # feed it all dots and new args, so the true ellipsis will catch the original
  # `...` args except the arg named `...`, + the new_dots
  call <- as.call(c(quote(f), quote(`...`), new_dots))
  # eval dot_editor call by taking function here, existing dots in exec env, and new args in caller
  all_dots <- eval(call, list(f=dot_editor, ... = exec_env$...), caller_env)
  class(all_dots) <- "dots"
  assign("...", all_dots, exec_env)
}

#' Evaluate an Expression with dots
#'
#' `with_dots(d, f(...))` is equivalent to `with(list(... = d), f(...))` but
#' shorter and quicker.
#' @param dots a "dots" object
#' @param expr an expression using `...`
#' @export
#' @examples
#' dots1 <- dots(letters, n =3)
#' dots1
#' with_dots(dots1, head(...))
#' dots2 <- dots(n =3)
#' dots2
#' with_dots(dots2, head(letters, ...))
with_dots <- function(dots, expr) {
  eval(substitute(expr), list(... = dots), parent.frame())
}


#' @export
`names<-.dots` <- function(x, value) {
  x <- dots_to_quos(x)
  names(x) <- value
  as_dots(x)
}


dots_to_quos <- function(x, ...) {
  with_dots(x, enquos(...))
}

#' convert object to dots
#'
#' @param x a list-like object, including a list of quosures. In the former case
#'   the environments will be set to the caller environment, in the latter
#'   case the environments will be preserved.
#' @export
as_dots <- function(x) {
  UseMethod("as_dots")
}

#' @export
as_dots.default <- function(x) {
  do.call(function(...) environment()$..., x)
}

#' @export
as_dots.quosures <- function(x) {
  fun <- function(x,nm) {
    arg <- list(quo_get_expr(x))
    names(arg) <- nm
    eval(as.call(c(quote(dots_in_env), quote(quo_get_env(x)),  arg)))
  }
  dot_list <- Map(fun, x, names(x))
  env <- environment()
  i <- 0
  makeActiveBinding("...", fun = function() {
    i <<- i + 1
    dot_list[[i]]
  }, env = env)
  dots_call <- as.call(c(quote(dots), replicate(length(dot_list), quote(`...`))))
  eval(dots_call)
}

