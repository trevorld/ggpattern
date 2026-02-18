#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
# TODO: deprecate this
GeomColPattern <- ggproto("GeomColPattern", GeomBarPattern)

#' @export
#' @rdname geom-docs
geom_col_pattern <- make_constructor(GeomColPattern, position = "stack", just = 0.5)
