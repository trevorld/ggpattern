
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file was copied (mostly untouched) from ggplot2 v3.3.0.9000
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interleave (or zip) multiple units into one vector
interleave <- function(...) UseMethod("interleave")
#' @export
interleave.unit <- function(...) {
  do.call("unit.c", do.call("interleave.default", lapply(list(...), as.list)))
}
#' @export
interleave.default <- function(...) {
  vectors <- list(...)

  # Check lengths
  lengths <- unique(setdiff(vapply(vectors, length, integer(1)), 1L))
  if (length(lengths) == 0) lengths <- 1
  if (length(lengths) > 1) abort("`lengths` must be below 1")

  # Replicate elements of length one up to correct length
  singletons <- vapply(vectors, length, integer(1)) == 1L
  vectors[singletons] <- lapply(vectors[singletons], rep, lengths)

  # Interleave vectors
  n <- lengths
  p <- length(vectors)
  interleave <- rep(1:n, each = p) + seq(0, p - 1) * n
  unlist(vectors, recursive = FALSE)[interleave]
}
