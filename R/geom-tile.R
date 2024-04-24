#' @rdname geom-docs
#' @export
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
    params = list2(
      linejoin = linejoin,
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
GeomTilePattern <- ggproto("GeomTilePattern", GeomRectPattern,
  extra_params = c("na.rm"),

  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% resolution(data$x, FALSE, TRUE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE, TRUE)

    transform(data,
              xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = defaults(
    aes(fill = "grey20", colour = NA, linewidth = 0.1, linetype = 1,
      alpha = NA, width = NA, height = NA),
    pattern_aesthetics
  ),

  required_aes = c("x", "y")
)
