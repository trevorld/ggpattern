#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Gradient colour scales
#'
#' See \code{ggplot2::scale_colour_gradient()} for more information
#'
#' `scale_*_gradient` creates a two colour gradient (low-high),
#' `scale_*_gradient2` creates a diverging colour gradient (low-mid-high),
#' `scale_*_gradientn` creates a n-colour gradient.
#'
#' @param space,...,na.value,aesthetics See \code{scales::seq_gradient_pal}, \code{scale_colour_hue}, \code{ggplot2::continuous_scale}
#' @param low,high Colours for low and high ends of the gradient.
#' @param guide Type of legend. Use `"colourbar"` for continuous
#'   colour bar, or `"legend"` for discrete colour legend.
#' @examples
#'   if (require("ggplot2")) {
#'     df <- data.frame(level = c("a", "b", "c", "d"),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = outcome),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_gradient()
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                  na.value = "grey50",
                                  guide = guide_colourbar(available_aes = "pattern_colour"),
                                  aesthetics = "pattern_colour") {
  continuous_scale(aesthetics, palette = seq_gradient_pal(low, high, space),
    na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                        na.value = "grey50",
                                        guide = guide_colourbar(available_aes = "pattern_fill"),
                                        aesthetics = "pattern_fill") {
  continuous_scale(aesthetics, palette = seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                        na.value = "grey50",
                                        guide = guide_colourbar(available_aes = "pattern_fill2"),
                                        aesthetics = "pattern_fill2") {
  continuous_scale(aesthetics, palette = seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @inheritParams scales::div_gradient_pal
#' @param midpoint The midpoint (in data value) of the diverging scale.
#'   Defaults to 0.
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                   midpoint = 0, space = "Lab", na.value = "grey50",
                                   guide = guide_colourbar(available_aes = "pattern_colour"),
                                   aesthetics = "pattern_colour") {
  continuous_scale(aesthetics, 
    palette = div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
    rescaler = mid_rescaler(mid = midpoint))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                         midpoint = 0, space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill"),
                                         aesthetics = "pattern_fill") {
  continuous_scale(aesthetics,
                   palette = div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                         midpoint = 0, space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill2"),
                                         aesthetics = "pattern_fill2") {
  continuous_scale(aesthetics,
                   palette = div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @inheritParams scales::gradient_n_pal
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                   guide = guide_colourbar(available_aes = "pattern_colour"),
                                   aesthetics = "pattern_colour", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics,
    palette = gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill"),
                                         aesthetics = "pattern_fill", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics,
                   palette = gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill2"),
                                         aesthetics = "pattern_fill2", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics,
                   palette = gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}
