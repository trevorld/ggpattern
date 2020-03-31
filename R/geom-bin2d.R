
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname geom_rect_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_bin2d_pattern <- function(mapping = NULL, data = NULL,
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
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}




if(FALSE) {
  library(ggplot2)

  ggplot(diamonds, aes(x, y)) + xlim(4, 10) + ylim(4, 10) +
    geom_bin2d_pattern(aes(pattern_density = ..density..), fill = 'white', bins = 5) +
    theme_bw() +
    labs(title = "ggpattern::geom_bin2d()")

}


