#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Viridis colour scales from viridisLite
#'
#' The `viridis` scales provide colour maps that are perceptually uniform in both
#' colour and black-and-white. They are also designed to be perceived by viewers
#' with common forms of colour blindness. See also
#' <https://bids.github.io/colormap/>.
#'
#' @param begin,end,alpha,direction,option,values,space,na.value,guide See [ggplot2::scale_colour_viridis_d()] for more information
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()],
#' [ggplot2::continuous_scale()], or [ggplot2::binned_scale()] to control name, limits, breaks,
#'   labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @examples
#'   if (require("ggplot2")) {
#'     df <- data.frame(level = c("a", "b", "c", "d"),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     # discrete 'viridis' palette
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_viridis_d()
#'     plot(gg)
#'
#'     # continuous 'viridis' palette
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = outcome),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_viridis_c()
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", aesthetics = "pattern_colour") {
  discrete_scale(
    aesthetics,
    palette = viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", aesthetics = "pattern_fill") {
  discrete_scale(
    aesthetics,
    palette = viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", aesthetics = "pattern_fill2") {
  discrete_scale(
    aesthetics,
    palette = viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                   direction = 1, option = "D", values = NULL,
                                   space = "Lab", na.value = "grey50",
                                   guide = guide_colourbar(available_aes = "pattern_colour"),
                                   aesthetics = "pattern_colour") {
  continuous_scale(
    aesthetics,
    palette = gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", values = NULL,
                                         space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill"),
                                         aesthetics = "pattern_fill") {
  continuous_scale(
    aesthetics,
    palette = gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", values = NULL,
                                         space = "Lab", na.value = "grey50",
                                         guide = guide_colourbar(available_aes = "pattern_fill2"),
                                         aesthetics = "pattern_fill2") {
  continuous_scale(
    aesthetics,
    palette = gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_viridis_d
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_colour_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
#                                    direction = 1, option = "D", values = NULL,
#                                    space = "Lab", na.value = "grey50",
#                                    guide = "coloursteps", aesthetics = "pattern_colour") {
#   binned_scale(
#     aesthetics,
#     palette = gradient_n_pal(
#       viridis_pal(alpha, begin, end, direction, option)(6),
#       values,
#       space
#     ),
#     na.value = na.value,
#     guide = guide,
#     ...
#   )
# }
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_viridis_d
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_fill_viridis_b <- function(..., alpha = 1, begin = 0, end = 1,
#                                  direction = 1, option = "D", values = NULL,
#                                  space = "Lab", na.value = "grey50",
#                                  guide = "coloursteps", aesthetics = "pattern_fill") {
#   binned_scale(
#     aesthetics,
#     palette = gradient_n_pal(
#       viridis_pal(alpha, begin, end, direction, option)(6),
#       values,
#       space
#     ),
#     na.value = na.value,
#     guide = guide,
#     ...
#   )
# }
