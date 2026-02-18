#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
GeomTilePattern <- ggproto(
  "GeomTilePattern", GeomRectPattern,
  extra_params = c("na.rm"),

  setup_data = GeomTile$setup_data,

  default_aes = defaults(aes(
	  fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
      colour = from_theme(colour %||% NA),
      linewidth = from_theme(0.4 * borderwidth),
      linetype = from_theme(bordertype),
      alpha = NA, width = 1, height = 1
	),
    pattern_aesthetics
  ),

  required_aes = c("x", "y")
)

#' @rdname geom-docs
#' @export
geom_tile_pattern <- make_constructor(GeomTilePattern)
