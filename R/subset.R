# FIXME:
#  add `strict` argument, FALSE by default, and non existent indices are ignored
#  if TRUE we fail when index doesn't exist
# not sure if the function should support logical indexing

#' Subset dots
#'
#' Use `[` to subset or reorder dots, this will not trigger any evaluation.
#' Use `[[` or `$` to evaluate an argument, note that `x` will not be modified.
#' @param x a "dots" object
#' @param i an index for `[` or `[[`
#' @param name a character index for `$`
#' @param ... ignored
#' @export
#' @name dot-subset
`[.dots` <- function(x, i, ...){
  n <- length(x)
  if (n == 0) return(x)
  dot_nms <- names(x)
  dot_list <- dot_split(x)[i]
  rm("...")
  env <- environment()
  i <- 0
  makeActiveBinding("...", fun = function() {
    i <<- i + 1
    dot_list[[i]]
  }, env = env)
  dots_call <- as.call(c(quote(dots), replicate(length(dot_list), quote(`...`))))
  eval(dots_call)
}


#' @export
#' @rdname dot-subset
`[[.dots` <- function(x, i,...){
  if (length(i) != 1) abort(c(
    "`i` must be of length 1.",
    x = sprintf("It has length %s.", length(i))
  ))
  with_dots(dot_split(x)[[i]], list(...)[[1]])
}

#' @export
#' @rdname dot-subset
`$.dots` <- function(x, name, ...) {
  with_dots(dot_split(x)[[name]], list(...)[[1]])
}

#' @export
`[[<-.dots` <- function(x, i, ..., value) {
  if (length(i) != 1) abort(c(
    "`i` must be of length 1.",
    x = sprintf("It has length %s.", length(i))
  ), call = quote(`[[<-.dots`()))

  if (is.logical(i)) {
    abort("`i` must not be logical", call = quote(`[[<-.dots`()))
  }
  if (is_ellipsis(value)) {
    abort(c(
      "`value` must not be of type '...'",
      i = "Use `[<-.dots` to assign lazy values to a dot arguments"
    ), call = quote(`[[<-.dots`()))
  }
  x <- dots_to_quos(x)
  value <- as_quosure(value, empty_env())
  if (is.numeric(i) && as.integer(i) > length(x) + 1) {
    abort(c(
      "Can't define dot arguments beyond the last with non-consecutive locations.",
      x = sprintf("Dots have length %s and you tried to define an argument at position %s", length(x), i)
    ), call = quote(`[[<-.dots`()))
  }

  x[i] <- list(value)
  as_dots(x)
}

#' @export
`$<-.dots` <- function(x, i, ..., value) {
  if (is_ellipsis(value)) {
    abort(c(
      "`value` must not be of type '...'",
      i = "Use `[<-.dots` to assign lazy values to a dot arguments"
    ), call = quote(`$<-.dots`()))
  }
  # FIXME: validate indices, forbid logical
  x <- dots_to_quos(x)
  value <- as_quosure(value, empty_env())
  x[i] <- list(value)
  as_dots(x)
}

#' @export
`[<-.dots` <- function(x, i, ..., value) {
  if (!is_ellipsis(value)) {
    abort(c(
      "`value` must be of type '...'",
      i = "Such object can be built with the `dots3::dots()` function",
      x = sprintf("`value` is of type '%s'", typeof(value)),
      i = "Use `[[<-.dots` or `$<-.dots` to assign a non lazy value to a dot argument"
    ), call = quote(`[<-.dots`()))
  }
  # FIXME: validate indices, handle logical
  if (length(value) != length(i)) {
    abort(c(
      "`value` must have the same length as the subset",
      x = sprintf("`value` has length %s, the subset has length %s", length(value), length(i))
    ), call = quote(`[<-.dots`()))
  }
  x <- dots_to_quos(x)
  value <- dots_to_quos(value)
  x[i] <- value
  as_dots(x)
}
