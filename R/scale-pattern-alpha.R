
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Alpha transparency scales
#'
#' See [ggplot2::scale_alpha()] for details.
#'
#' @param ... Other arguments passed on to [ggplot2::continuous_scale()], [ggplot2::binned_scale()],
#'   or [ggplot2::discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of alpha values. Must lie between 0 and 1.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level, pattern_alpha = outcome),
#'         pattern_density = 0.6,
#'         pattern_size = 1.5,
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black',
#'         linewidth = 1.5
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_alpha() +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'stripe'"
#'       )
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_alpha_continuous <- function(..., range = c(0.1, 1)) {
  continuous_scale("pattern_alpha", palette = rescale_pal(range), ...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_alpha_continuous
#' @export
#-----------------------------------------------------------------------------
scale_pattern_alpha <- scale_pattern_alpha_continuous

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_alpha_continuous
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_alpha_binned <- function(..., range = c(0.1, 1)) {
#   binned_scale("pattern_alpha", palette = rescale_pal(range), ...)
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
    palette = function(n) seq(range[1], range[2], length.out = n),
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
