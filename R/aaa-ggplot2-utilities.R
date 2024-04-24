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
detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}
is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}
# Wrapper around is.finite to handle list cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}
is.waive <- function(x) inherits(x, "waiver")

binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}

# Wrapping vctrs data_frame constructor with no name repair
data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

# Wrapping unique0() to accept NULL
unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)
