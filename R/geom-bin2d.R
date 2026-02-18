#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @include geom-tile.R
#' @export
GeomBin2dPattern <- ggproto("GeomBin2dPattern", GeomTilePattern)

#' @rdname geom-docs
#' @export
geom_bin_2d_pattern <- make_constructor(GeomBin2dPattern, stat = "bin2d")

#' @rdname geom-docs
#' @export
geom_bin2d_pattern <- geom_bin_2d_pattern
