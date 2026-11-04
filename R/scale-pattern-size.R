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
                                  limits = NULL, range = c(1, 6),
                                  trans = deprecated(), guide = "legend",
                                  ..., transform = "identity") {
  if (lifecycle::is_present(trans)) {{
     lifecycle::deprecate_warn('1.1.1',
                               'scale_pattern_size_continuous(trans)',
                               'scale_pattern_size_continuous(transform)')
     transform <- trans
  }}
  continuous_scale("pattern_size", palette = area_pal(range), name = name,
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
scale_pattern_size_ordinal <- function(..., range = c(2, 6)) {
  force(range)

  discrete_scale(
    "pattern_size",
    palette = function(n) {
      area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
      sqrt(area)
    },
    ...
  )
}
