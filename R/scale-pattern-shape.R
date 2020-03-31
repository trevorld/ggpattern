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
#'   `FALSE`?
#' @param ... other arguments passed to discrete_scale()
#'
#' @export
#-----------------------------------------------------------------------------
scale_pattern_shape <- function(..., solid = TRUE) {
  discrete_scale("pattern_shape", "pattern_shape_d", shape_pal(solid), ...)
}

#  #-----------------------------------------------------------------------------
#  #' @rdname scale_pattern_shape
#  #' @export
#  #-----------------------------------------------------------------------------
#  scale_pattern_shape_binned <- function(..., solid = TRUE) {
#    binned_scale("pattern_shape", "pattern_shape_b", binned_pal(shape_pal(solid)), ...)
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
