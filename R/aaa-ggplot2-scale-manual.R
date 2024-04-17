
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file was copied (mostly untouched) from ggplot2 v3.3.0.9000
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

manual_scale <- function(aesthetic, values = NULL, breaks = waiver(), ...) {
  # check for missing `values` parameter, in lieu of providing
  # a default to all the different scale_*_manual() functions
  if (is_missing(values)) {
    values <- NULL
  } else {
    force(values)
  }

  # order values according to breaks
  if (is.vector(values) && is.null(names(values)) && !is.waive(breaks) &&
      !is.null(breaks)) {
    if (length(breaks) != length(values)) {
      abort(glue("
        Differing number of values and breaks in manual scale.
        {length(values)} values provided compared to {length(breaks)} breaks.
      "))
    }
    names(values) <- breaks
  }

  pal <- function(n) {
    if (n > length(values)) {
      abort(glue("Insufficient values in manual scale. {n} needed but only {length(values)} provided."))
    }
    values
  }
  discrete_scale(aesthetic, palette = pal, breaks = breaks, ...)
}
