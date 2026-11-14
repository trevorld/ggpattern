#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomHexPattern <- ggproto("GeomHexPattern", GeomHex,
  default_aes = defaults(aes(
    colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper)),
    linewidth = from_theme(borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA
  ),
    pattern_aesthetics
  ),

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", linemitre = 10) {
    grob <- ggproto_parent(GeomHex, self)$draw_group(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    # one hexagon per row
    ggname("geom_hex_pattern", with_pattern_aes(grob, data))
  },

  draw_key = draw_key_polygon_pattern
)

#' @rdname geom-docs
#' @export
geom_hex_pattern <- make_constructor(GeomHexPattern, stat = "binhex")
