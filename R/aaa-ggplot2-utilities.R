#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file was copied (mostly untouched) from ggplot2 v3.3.0.9000
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @param character vector of required aesthetics
# @param character vector of present aesthetics
# @param name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name) {
  if (is.null(required)) return()

  required <- strsplit(required, "|", fixed = TRUE)
  if (any(vapply(required, length, integer(1)) > 1)) {
    required <- lapply(required, rep_len, 2)
    required <- list(
      vapply(required, `[`, character(1), 1),
      vapply(required, `[`, character(1), 2)
    )
  } else {
    required <- list(unlist(required))
  }
  missing_aes <- lapply(required, setdiff, present)
  if (any(vapply(missing_aes, length, integer(1)) == 0)) return()

  abort(glue(
    "{name} requires the following missing aesthetics: ",
    glue_collapse(lapply(missing_aes, glue_collapse, sep = ", ", last = " and "), sep = " or ")
  ))
}

# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

#' A waiver object.
#'
#' A waiver is a "flag" object, similar to `NULL`, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (`NULL`) and
#' displaying a default value calculated elsewhere (`waiver()`)
#'
#' @export
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")

binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}

is.formula <- function(x) inherits(x, "formula")

warning_wrap <- function(...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") - 2)
  warn(glue_collapse(wrapped, "\n", last = "\n"))
}

