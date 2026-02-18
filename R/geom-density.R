#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.R
GeomDensityPattern <- ggproto(
  "GeomDensityPattern", GeomAreaPattern,
  default_aes = defaults(aes(
        colour = from_theme(colour %||% ink),
		fill = from_theme(fill %||% NA),
		weight = 1,
		alpha = NA,
        linewidth = from_theme(linewidth),
		linetype = from_theme(linetype)
	),
    pattern_aesthetics
  )
)
#' @rdname geom-docs
#' @export
geom_density_pattern <- make_constructor(
  GeomDensityPattern, stat = "density", outline.type = "upper",
  checks = exprs(
    outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))
  )
)
