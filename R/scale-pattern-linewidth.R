#-----------------------------------------------------------------------------
#' Scales for line width
#'
#' `scale_pattern_linewidth()` scales the width of lines (e.g. the stripes of the "stripe" pattern).
#' It is the pattern analogue of [ggplot2::scale_linewidth()].
#' The `pattern_linewidth` aesthetic supersedes using `pattern_size` for line widths
#' (if `pattern_linewidth` is unset it falls back to `pattern_size`).
#' Use [scale_pattern_size()] for the fontsize of the "text" pattern.
#'
#' @param name,breaks,labels,limits,guide,...,transform See [ggplot2::scale_linewidth()] for more information
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum line width after transformation.
#'   If `NULL` (and `palette` is `NULL`) uses the `palette.pattern_linewidth.<type>` theme element
#'   (see [pattern_palettes]) else `c(1, 6)` (`c(2, 6)` for the ordinal scale).
#' @param palette A palette function e.g. [scales::pal_rescale()] for a continuous scale
#'   or a `function(n)` returning `n` values for a discrete scale.  Takes precedence over `range`.
#'   For a vector of values use `range` or [scale_pattern_linewidth_manual()].
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'circle' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level,
#'             linewidth = outcome, pattern_linewidth = outcome),
#'         pattern_density = 0.4,
#'         pattern_spacing = 0.3,
#'         pattern = 'circle',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_linewidth() +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'circle'"
#'       )
#'     plot(gg)
#'   }
#'
#' @export
#-----------------------------------------------------------------------------
scale_pattern_linewidth_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                               limits = NULL, range = NULL,
                                               guide = "legend",
                                               ..., transform = "identity", palette = NULL) {
  continuous_scale("pattern_linewidth",
    palette = as_scale_palette(palette, range, scales::rescale_pal, "continuous", "pattern_linewidth", "range"),
    fallback.palette = scales::rescale_pal(c(1, 6)),
    name = name,
    breaks = breaks, labels = labels, limits = limits, transform = transform,
    guide = guide, ...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_linewidth_continuous
#' @export
#-----------------------------------------------------------------------------
scale_pattern_linewidth <- scale_pattern_linewidth_continuous

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_linewidth_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_pattern_linewidth_discrete <- function(...) {
  scale_pattern_linewidth_ordinal(...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_linewidth_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_pattern_linewidth_ordinal <- function(..., range = NULL, palette = NULL) {
  discrete_scale(
    "pattern_linewidth",
    palette = as_scale_palette(palette, range, pal_seq_range, "discrete", "pattern_linewidth", "range"),
    fallback.palette = pal_seq_range(c(2, 6)),
    ...
  )
}
