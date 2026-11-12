#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectPattern <- ggproto( "GeomRectPattern", GeomRect,
  default_aes = defaults(aes(
	colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper, 0.35)),
    linewidth = from_theme(borderwidth), linetype = from_theme(bordertype),
    alpha = NA
  ),
    pattern_aesthetics
  ),

  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    grob <- ggproto_parent(GeomRect, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin
    )
    # one rectangle (or polygon if the coord is non-linear) per row
    ggname("geom_rect_pattern", with_pattern_rows(grob, data))
  },

  draw_key = draw_key_polygon_pattern,
)

#' @export
#' @rdname geom-docs
geom_rect_pattern <- make_constructor(GeomRectPattern)
