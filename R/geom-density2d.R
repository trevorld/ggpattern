#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-polygon.R
#' @export
GeomDensity2dFilledPattern <- ggproto("GeomDensity2dFilledPattern", GeomPolygonPattern)

#' @rdname geom-docs
#' @export
geom_density_2d_filled_pattern <- function(mapping = NULL, data = NULL,
                                           stat = "density_2d_filled", position = "identity",
                                           ...,
                                           contour_var = "density",
                                           na.rm = FALSE,
                                           show.legend = NA,
                                           inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity2dFilledPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      contour = TRUE,
      contour_var = contour_var,
      ...
    )
  )
}

#' @rdname geom-docs
#' @export
geom_density2d_filled_pattern <- geom_density_2d_filled_pattern
