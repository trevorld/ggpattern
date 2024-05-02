#' @rdname geom-docs
#' @export
geom_bin_2d_pattern <- function(mapping = NULL, data = NULL,
                       stat = "bin2d", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTilePattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-docs
#' @export
geom_bin2d_pattern <- geom_bin_2d_pattern
