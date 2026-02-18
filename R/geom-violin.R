#' @rdname geom-docs
#' @export
geom_violin_pattern <- function(mapping = NULL, data = NULL,
                                stat = "ydensity", position = "dodge",
                                ...,
                                trim = TRUE,
                                bounds = c(-Inf, Inf),
                                quantile.colour = NULL,
                                quantile.color = NULL,
                                quantile.linetype = 0L,
                                quantile.linewidth = NULL,
                                draw_quantiles = deprecated(),
                                scale = "area",
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {

  extra <- list()
  if (lifecycle::is_present(draw_quantiles)) {
    deprecate_soft(
	  "1.3.1",
      what = "geom_violin_pattern(draw_quantiles)",
      with = "geom_violin_pattern(quantile.linetype)"
	)
    stopifnot(is.numeric(draw_quantiles))

	# Pass on to stat when stat accepts 'quantiles'
    # stat <- validate_subclass(stat, "Stat", current_call(), caller_env())
    if ("quantiles" %in% stat$parameters()) {
      extra$quantiles <- draw_quantiles
    }

    # Turn on quantile lines
    if (!is.null(quantile.linetype)) {
      quantile.linetype <- max(quantile.linetype, 1)
    }
  }

  quantile_gp <- list(
    colour = quantile.color %||% quantile.colour,
    linetype = quantile.linetype,
    linewidth = quantile.linewidth
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomViolinPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      trim = trim,
      scale = scale,
      na.rm = na.rm,
      orientation = orientation,
      bounds = bounds,
      quantile_gp = quantile_gp,
      !!!extra,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomViolinPattern <- ggproto("GeomViolinPattern", GeomViolin,

  draw_group = function(self, data, ..., quantile_gp = list(linetype = 0), flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # Find the points for the line to go all the way around
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- vec_rbind(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- vec_rbind(newdata, newdata[1,])
    newdata <- flip_data(newdata, flipped_aes)

	violin_grob <- GeomPolygonPattern$draw_panel(newdata, ...)

    if (!"quantile" %in% names(newdata) ||
        all(quantile_gp$linetype == 0) ||
        all(quantile_gp$linetype == "blank")) {
      return(ggname("geom_violin_pattern", violin_grob))
    }

    # Draw quantiles if requested, so long as there is non-zero y range
      quantiles <- newdata[!is.na(newdata$quantile),]
      quantiles$group <- match(quantiles$quantile, unique(quantiles$quantile))
      quantiles$linetype <- quantile_gp$linetype %||% quantiles$linetype
      quantiles$linewidth <- quantile_gp$linewidth %||% quantiles$linewidth
      quantiles$colour <- quantile_gp$colour %||% quantiles$colour

      quantile_grob <- if (nrow(quantiles) == 0) {
        zeroGrob()
      } else {
        GeomPath$draw_panel(quantiles, ...)
      }

      ggname("geom_violin_pattern", grobTree(violin_grob, quantile_grob))
  },

  draw_key = draw_key_polygon_pattern,

  default_aes = defaults(aes(
		weight = 1,
        colour = from_theme(colour %||% col_mix(ink, paper, 0.2)),
        fill = from_theme(fill %||% paper),
        linewidth = from_theme(borderwidth),
        linetype = from_theme(bordertype),
        alpha = NA,
		width = 0.9
	),
    pattern_aesthetics
  )
)
