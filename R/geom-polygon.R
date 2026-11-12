#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolygonPattern <- ggproto("GeomPolygonPattern", GeomPolygon,
  draw_panel = function(self, data, panel_params, coord, rule = "evenodd",
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    grob <- ggproto_parent(GeomPolygon, self)$draw_panel(
      data, panel_params, coord, rule = rule,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    # one polygon (or path) per group (sorted by group)
    ggname("geom_polygon_pattern", with_pattern_rows(grob, first_rows_by_group(data)))
  },

  default_aes = defaults(aes(
		colour = from_theme(colour %||% NA),
		fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
		linewidth = from_theme(borderwidth),
		linetype = from_theme(bordertype),
		alpha = NA, subgroup = NULL
	),
    pattern_aesthetics
  ),


  draw_key = draw_key_polygon_pattern,
)

#' @rdname geom-docs
#' @export
geom_polygon_pattern <- make_constructor(GeomPolygonPattern)
