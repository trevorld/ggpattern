#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Evenly spaced colours for discrete data
#'
#' This is the default colour scale for categorical variables. It maps each
#' level to an evenly spaced hue on the colour wheel. It does not generate
#' colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param h,c,l,h.start,direction,... See \code{ggplot2::scale_colour_hue}
#' @examples
#'   if (require("ggplot2")) {
#'     df <- data.frame(level = c("a", "b", "c", "d"),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_hue()
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                             direction = 1, na.value = "grey50", aesthetics = "pattern_colour") {
  discrete_scale(aesthetics, 
                 palette = hue_pal(h, c, l, h.start, direction),
                 na.value = na.value,
                 ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_hue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                   direction = 1, na.value = "grey50", aesthetics = "pattern_fill") {
  discrete_scale(aesthetics, 
                 palette = hue_pal(h, c, l, h.start, direction),
                 na.value = na.value,
                 ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_hue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                   direction = 1, na.value = "grey50", aesthetics = "pattern_fill2") {
  discrete_scale(aesthetics, 
                 palette = hue_pal(h, c, l, h.start, direction),
                 na.value = na.value,
                 ...)
}
