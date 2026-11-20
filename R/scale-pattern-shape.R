#' Scales for shapes, aka glyphs
#'
#' `scale_pattern_shape` maps discrete variables to six easily discernible shapes.
#' If you have more than six levels, you will get a warning message, and the
#' seventh and subsequence levels will not appear on the plot. Use
#' [scale_pattern_shape_manual()] to supply your own values. You can not map
#' a continuous variable to shape unless `scale_pattern_shape_binned()` is used. Still,
#' as shape has no inherent order, this use is not advised..
#'

#-----------------------------------------------------------------------------
#' Scales for area or radius
#'
#' @param solid Should the shapes be solid, `TRUE`, or hollow,
#'   `FALSE`?  If `NULL` (and `palette` is `NULL`) uses the `palette.pattern_shape.discrete`
#'   theme element (see [pattern_palettes]) else solid shapes.
#' @param palette A palette function or a character vector of shapes (see [scales::pal_manual()]).
#'   For numeric (`pch`) shapes use e.g. `palette = scales::pal_manual(c(1, 2, 3))`
#'   or [scale_pattern_shape_manual()].
#'   Takes precedence over `solid`.
#' @param ... other arguments passed to discrete_scale()
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'pch' pattern example
#'     gg <- ggplot(mtcars, aes(as.factor(cyl), mpg)) +
#'       geom_violin_pattern(aes(fill = as.factor(cyl),
#'                               pattern_shape = as.factor(cyl)),
#'         pattern = 'pch',
#'         pattern_density = 0.3,
#'         pattern_angle = 0,
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_shape() +
#'       labs(
#'         title    = "ggpattern::geom_violin_pattern()",
#'         subtitle = "pattern = 'pch'"
#'       )
#'     plot(gg)
#'   }
#'
#' @export
#-----------------------------------------------------------------------------
scale_pattern_shape <- function(..., solid = NULL, palette = NULL) {
  discrete_scale("pattern_shape",
                 palette = as_scale_palette(palette, solid, shape_pal, "discrete", "pattern_shape"),
                 fallback.palette = shape_pal(),
                 ...)
}

#  #-----------------------------------------------------------------------------
#  #' @rdname scale_pattern_shape
#  #' @export
#  #-----------------------------------------------------------------------------
#  scale_pattern_shape_binned <- function(..., solid = TRUE) {
#    binned_scale("pattern_shape", palette = binned_pal(shape_pal(solid)), ...)
#  }

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_shape
#' @export
#-----------------------------------------------------------------------------
scale_pattern_shape_discrete <- scale_pattern_shape

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_shape
#' @export
#-----------------------------------------------------------------------------
scale_pattern_shape_ordinal <- function(...) {
  warn("Using shapes for an ordinal variable is not advised")
  scale_pattern_shape(...)
}

#-----------------------------------------------------------------------------
#' @rdname scale_pattern_shape
#' @export
#-----------------------------------------------------------------------------
scale_pattern_shape_continuous <- function(...) {
  abort("A continuous variable can not be mapped to shape")
}
