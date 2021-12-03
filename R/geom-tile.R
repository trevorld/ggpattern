#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_tile_pattern <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              linejoin = "mitre",
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
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#' @include geom-rect.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomTilePattern <- ggproto(
  "GeomTilePattern", GeomRectPattern,
  extra_params = c("na.rm"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
              xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      fill     = "grey20",
      colour   = NA,
      size     = 0.1,
      linetype = 1,
      alpha    = NA,
      width    = NA,
      height   = NA
    )
  ),

  required_aes = c("x", "y")
)


if (FALSE) {
  library(ggplot2)

  df <- data.frame(
    x = rep(c(2, 5, 7, 9, 12), 2),
    y = rep(c(1, 2), each = 5),
    z = factor(rep(1:5, each = 2)),
    w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
  )

  ggplot(df, aes(x, y)) +
    geom_tile_pattern(aes(fill = z, pattern = z), colour = "grey50") +
    theme_bw() +
    labs(title = "ggpattern::geom_tile_pattern()")

}


