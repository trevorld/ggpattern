
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Alpha transparency scales
#'
#' See \code{ggplot2::scale_alpha()} for details.
#'
#' @param ... Other arguments passed on to [continuous_scale()], [binned_scale],
#'   or [discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of alpha values. Must lie between 0 and 1.
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_alpha_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale("pattern_alpha", "pattern_alpha_c", rescale_pal(range), ...)
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_alpha_continuous
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_alpha_binned <- function(..., range = c(0.1, 1)) {
#   binned_scale("pattern_alpha", "pattern_alpha_b", rescale_pal(range), ...)
# }


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_alpha_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_alpha_discrete <- function(...) {
  warn("scale_pattern_alpha_discrete(): Using alpha for a discrete variable is not advised.")
  scale_pattern_alpha_ordinal(...)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_alpha_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_alpha_ordinal <- function(..., range = c(0.1, 1)) {
  discrete_scale(
    "pattern_alpha",
    "pattern_alpha_d",
    function(n) seq(range[1], range[2], length.out = n),
    ...
  )
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_alpha_continuous
# #' @export
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_alpha_datetime <- function(..., range = c(0.1, 1)) {
#   datetime_scale("pattern_alpha", "time", palette = rescale_pal(range), ...)
# }
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_alpha_continuous
# #' @export
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_alpha_date <- function(..., range = c(0.1, 1)){
#   datetime_scale("pattern_alpha", "date", palette = rescale_pal(range), ...)
# }
