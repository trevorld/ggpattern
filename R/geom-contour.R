#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-polygon.R
#' @export
GeomContourFilledPattern <- ggproto("GeomContourFilledPattern", GeomPolygonPattern)

#' @rdname geom-docs
#' @export
geom_contour_filled_pattern <- make_constructor(
  GeomContourFilledPattern, stat = "contour_filled",
  bins = NULL, binwidth = NULL, breaks = NULL
)
