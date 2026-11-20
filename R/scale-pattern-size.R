#-----------------------------------------------------------------------------
#' Scales for fontsize (or legacy line width)
#'
#' `scale_pattern_size()` scales the fontsize of the "text" pattern.
#' Using `pattern_size` for the line widths of other patterns is superseded by
#' the `pattern_linewidth` aesthetic and [scale_pattern_linewidth()]
#' (an unset `pattern_linewidth` still falls back to `pattern_size` for backwards compatibility).
#'
#' @param name,breaks,labels,limits,trans,guide,...,transform See [ggplot2::scale_size()] for more information
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size after transformation.
#'   If `NULL` (and `palette` is `NULL`) uses the `palette.pattern_size.<type>` theme element
#'   (see [pattern_palettes]) else `c(1, 6)` (`c(2, 6)` for the ordinal scale).
#' @param palette A palette function e.g. [scales::pal_rescale()] for a continuous scale
#'   or a `function(n)` returning `n` values for a discrete scale.  Takes precedence over `range`.
#'   For a vector of values use `range` or [scale_pattern_size_manual()].
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'text' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_colour = level, pattern_size = outcome),
#'         pattern_spacing = 0.08,
#'         pattern_angle   = 0,
#'         pattern = 'text',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_size(range = c(6, 18)) +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'text'"
#'       )
#'     plot(gg)
#'   }
#'
#' @export
#-----------------------------------------------------------------------------
scale_pattern_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = NULL,
                                  trans = deprecated(), guide = "legend",
                                  ..., transform = "identity", palette = NULL) {
  if (lifecycle::is_present(trans)) {{
     lifecycle::deprecate_warn('1.1.1',
                               'scale_pattern_size_continuous(trans)',
                               'scale_pattern_size_continuous(transform)')
     transform <- trans
  }}
  continuous_scale("pattern_size",
    palette = as_scale_palette(palette, range, area_pal, "continuous", "pattern_size", "range"),
    fallback.palette = area_pal(c(1, 6)),
    name = name,
    breaks = breaks, labels = labels, limits = limits, transform = transform,
    guide = guide, ...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_size_continuous
#' @export
#-----------------------------------------------------------------------------
scale_pattern_size <- scale_pattern_size_continuous

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_size_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_pattern_size_discrete <- function(...) {
  scale_pattern_size_ordinal(...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_size_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_pattern_size_ordinal <- function(..., range = NULL, palette = NULL) {
  discrete_scale(
    "pattern_size",
    palette = as_scale_palette(palette, range, pal_seq_area, "discrete", "pattern_size", "range"),
    fallback.palette = pal_seq_area(c(2, 6)),
    ...
  )
}

# Discrete palette of sizes whose areas are evenly spaced within `range`
pal_seq_area <- function(range) {
  force(range)
  function(n) {
    area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
    sqrt(area)
  }
}
