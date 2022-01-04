
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sequential grey colour scales
#'
#' Based on [gray.colors()]. This is black and white equivalent
#' of [scale_pattern_colour_gradient()].
#'
#' @param ...,start,end,na.value,aesthetics See
#'        \code{ggplot2::scale_colour_grey} for more information
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_grey <- function(..., start = 0.2, end = 0.8, na.value = "red", aesthetics = "pattern_colour") {
  discrete_scale(aesthetics, "grey", grey_pal(start, end),
    na.value = na.value, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_grey
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_grey <- function(..., start = 0.2, end = 0.8, na.value = "red", aesthetics = "pattern_fill") {
  discrete_scale(aesthetics, "grey", grey_pal(start, end),
                 na.value = na.value, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_grey
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_grey <- function(..., start = 0.2, end = 0.8, na.value = "red", aesthetics = "pattern_fill2") {
  discrete_scale(aesthetics, "grey", grey_pal(start, end),
                 na.value = na.value, ...)
}
