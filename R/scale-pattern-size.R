
#-----------------------------------------------------------------------------
#' Scales for area or radius
#'
#' @param name,breaks,labels,limits,trans,guide See \code{ggplot2::scale_size} for more information
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'circle' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level,
#'             size = outcome, pattern_size = outcome),
#'         pattern_density = 0.4,
#'         pattern_spacing = 0.3,
#'         pattern = 'circle',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_size() +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'circle'"
#'       )
#'     plot(gg)
#'   }
#'
#' @export
#-----------------------------------------------------------------------------
scale_pattern_size_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                  limits = NULL, range = c(1, 6),
                                  trans = "identity", guide = "legend") {
  continuous_scale("pattern_size", "area", area_pal(range), name = name,
    breaks = breaks, labels = labels, limits = limits, trans = trans,
    guide = guide)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_size_continuous
#' @export
#-----------------------------------------------------------------------------
scale_pattern_size <- scale_pattern_size_continuous


# #-----------------------------------------------------------------------------
# #' @rdname scale_pattern_size_continuous
# #' @export
# #-----------------------------------------------------------------------------
# scale_size_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
#                               limits = NULL, range = c(1, 6), n.breaks = NULL,
#                               nice.breaks = TRUE, trans = "identity", guide = "bins") {
#   binned_scale("size", "area_b", area_pal(range), name = name,
#                breaks = breaks, labels = labels, limits = limits, trans = trans,
#                n.breaks = n.breaks, nice.breaks = nice.breaks, guide = guide)
# }


#-----------------------------------------------------------------------------
#' @rdname scale_pattern_size_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_pattern_size_discrete <- function(...) {
  # warn("Using pattern_size for a discrete variable is not advised.")
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
    "pattern_size_d",
    function(n) {
      area <- seq(range[1] ^ 2, range[2] ^ 2, length.out = n)
      sqrt(area)
    },
    ...
  )
}

