
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scale for line patterns
#'
#' Default line types based on a set supplied by Richard Pearson,
#' University of Manchester. Continuous values can not be mapped to
#' line types.
#'
#' @param ... see \code{ggplot2::scale_linetype} for more information
#' @param na.value The linetype to use for `NA` values.
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype <- function(..., na.value = "blank") {
  discrete_scale("pattern_linetype", "pattern_linetype_d", linetype_pal(),
    na.value = na.value, ...)
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_linetype
# #' @export
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_linetype_binned <- function(..., na.value = "blank") {
#   binned_scale("pattern_linetype", "pattern_linetype_b", binned_pal(linetype_pal()), ...)
# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_linetype
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype_continuous <- function(...) {
  abort("A continuous variable can not be mapped to pattern_linetype")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_linetype
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype_discrete <- scale_pattern_linetype
