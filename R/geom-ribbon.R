#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRibbonPattern <- ggproto("GeomRibbonPattern", GeomRibbon,
  default_aes = defaults(aes(
	colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
    linewidth = from_theme(borderwidth),
	linetype = from_theme(bordertype),
    alpha = NA
	),
    pattern_aesthetics
  ),

  draw_key = draw_key_polygon_pattern,

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "round", linemitre = 10, na.rm = FALSE,
                        flipped_aes = FALSE, outline.type = "both") {
    # `GeomRibbon$draw_group()` checks its own aesthetics are constant
    pattern_params <- unique0(data[names(data) %in% names(pattern_aesthetics)])
    if (nrow(pattern_params) > 1) {
      cli::cli_abort("Pattern aesthetics can not vary along a ribbon.")
    }

    grob <- ggproto_parent(GeomRibbon, self)$draw_group(
      data, panel_params, coord, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre, na.rm = na.rm,
      flipped_aes = flipped_aes, outline.type = outline.type
    )
    # one polygon per run of non-missing values: all with the same pattern
    ggname("geom_ribbon_pattern", with_pattern_aes(grob, data[1L, , drop = FALSE]))
  },

  rename_size = TRUE
)

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAreaPattern <- ggproto("GeomAreaPattern", GeomRibbonPattern,

	required_aes = c("x", "y"),

	setup_params = function(self, data, params) ggproto_parent(GeomArea, self)$setup_params(data, params),

	setup_data = function(self, data, params) ggproto_parent(GeomArea, self)$setup_data(data, params)
)

#' @rdname geom-docs
#' @export
geom_ribbon_pattern <- make_constructor(
  GeomRibbonPattern, orientation = NA,
  checks = exprs(
    outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))
  )
)

#' @rdname geom-docs
#' @export
geom_area_pattern <- make_constructor(
  GeomAreaPattern, stat = "align", position = "stack",
  orientation = NA, outline.type = "upper",
  checks = exprs(
    outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))
  )
)
