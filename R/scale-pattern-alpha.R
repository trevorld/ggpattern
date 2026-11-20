
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Alpha transparency scales
#'
#' See [ggplot2::scale_alpha()] for details.
#'
#' @param ... Other arguments passed on to [ggplot2::continuous_scale()], [ggplot2::binned_scale()],
#'   or [ggplot2::discrete_scale()] as appropriate, to control name, limits,
#'   breaks, labels and so forth.
#' @param range Output range of alpha values. Must lie between 0 and 1.
#'   If `NULL` (and `palette` is `NULL`) uses the `palette.pattern_alpha.<type>` theme element
#'   (see [pattern_palettes]) else `c(0.1, 1)`.
#' @param palette A palette function e.g. [scales::pal_rescale()] for a continuous scale
#'   or a `function(n)` returning `n` values for a discrete scale.  Takes precedence over `range`.
#'   For a vector of values use `range` or [scale_pattern_alpha_manual()].
#' @examples
#'   if (require("ggplot2")) {
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level, pattern_alpha = outcome),
#'         pattern_density = 0.6,
#'         pattern_linewidth = 1.5,
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
scale_pattern_alpha_continuous <- function(..., range = NULL, palette = NULL) {
  continuous_scale("pattern_alpha",
                   palette = as_scale_palette(palette, range, rescale_pal, "continuous", "pattern_alpha", "range"),
                   fallback.palette = rescale_pal(c(0.1, 1)),
                   ...)
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
scale_pattern_alpha_ordinal <- function(..., range = NULL, palette = NULL) {
  discrete_scale(
    "pattern_alpha",
    palette = as_scale_palette(palette, range, pal_seq_range, "discrete", "pattern_alpha", "range"),
    fallback.palette = pal_seq_range(c(0.1, 1)),
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
