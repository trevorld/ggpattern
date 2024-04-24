#' @export
#' @rdname geom-docs
geom_col_pattern <- function(mapping = NULL, data = NULL,
                             position = "stack",
                             ...,
                             just = 0.5,
                             width = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomColPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      just = just,
      width = width,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
# TODO: deprecate this
GeomColPattern <- ggproto("GeomColPattern", GeomBarPattern)
