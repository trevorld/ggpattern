#' @rdname geom-docs
#' @export
geom_boxplot_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "boxplot", position = "dodge2",
                                 ...,
                                 outliers = TRUE,
                                 outlier.colour = NULL,
                                 outlier.color = NULL,
                                 outlier.fill = NULL,
                                 outlier.shape = NULL,
                                 outlier.size = NULL,
                                 outlier.stroke = 0.5,
                                 outlier.alpha = NULL,
                                 whisker.colour = NULL,
                                 whisker.color = NULL,
                                 whisker.linetype = NULL,
                                 whisker.linewidth = NULL,
                                 staple.colour = NULL,
                                 staple.color = NULL,
                                 staple.linetype = NULL,
                                 staple.linewidth = NULL,
                                 median.colour = NULL,
                                 median.color = NULL,
                                 median.linetype = NULL,
                                 median.linewidth = NULL,
                                 box.colour = NULL,
                                 box.color = NULL,
                                 box.linetype = NULL,
                                 box.linewidth = NULL,
                                 notch = FALSE,
                                 notchwidth = 0.5,
                                 staplewidth = 0,
                                 varwidth = FALSE,
                                 na.rm = FALSE,
                                 orientation = NA,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  outlier_gp <- list(
    colour = outlier.color %||% outlier.colour,
    fill = outlier.fill,
    shape = outlier.shape,
    size = outlier.size,
    stroke = outlier.stroke,
    alpha = outlier.alpha
  )

  whisker_gp <- list(
    colour = whisker.color %||% whisker.colour,
    linetype = whisker.linetype,
    linewidth = whisker.linewidth
  )

  staple_gp <- list(
    colour = staple.color %||% staple.colour,
    linetype = staple.linetype,
    linewidth = staple.linewidth
  )

  median_gp <- list(
    colour = median.color %||% median.colour,
    linetype = median.linetype,
    linewidth = median.linewidth
  )

  box_gp <- list(
    colour = box.color %||% box.colour,
    linetype = box.linetype,
    linewidth = box.linewidth
  )

  stopifnot(is.numeric(staplewidth))
  stopifnot(is.logical(outliers))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplotPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      outliers = outliers,
      outlier_gp = outlier_gp,
      whisker_gp = whisker_gp,
      staple_gp = staple_gp,
      median_gp = median_gp,
      box_gp = box_gp,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomBoxplotPattern <- ggproto("GeomBoxplotPattern", GeomBoxplot,

  setup_params = function(data, params) {
    if ("fatten" %in% names(params)) {
      deprecate_soft(
		"1.3.1", "geom_boxplot_pattern(fatten)",
        "geom_boxplot_pattern(median.linewidth)"
	  )
    } else {
      # For backward compatibility reasons
      params$fatten <- 2
    }
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier_gp = NULL,
                        whisker_gp = NULL, staple_gp = NULL, median_gp = NULL,
                        box_gp = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE) {
    grob <- ggproto_parent(GeomBoxplot, self)$draw_group(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin,
      fatten = fatten, outlier_gp = outlier_gp, whisker_gp = whisker_gp,
      staple_gp = staple_gp, median_gp = median_gp, box_gp = box_gp,
      notch = notch, notchwidth = notchwidth, staplewidth = staplewidth,
      varwidth = varwidth, flipped_aes = flipped_aes
    )
    # the box is the only area (outliers, whiskers, staples, and median have none)
    ggname("geom_boxplot_pattern", with_pattern_rows(grob, data))
  },

  # {covr} can't cover `draw_key_boxplot_pattern()` if directly assign it
  draw_key = function(data, params, size) draw_key_boxplot_pattern(data, params, size),

  default_aes = defaults(aes(
	  weight = 1, colour = from_theme(colour %||% col_mix(ink, paper, 0.2)),
      fill = from_theme(fill %||% paper), size = from_theme(pointsize),
      alpha = NA, shape = from_theme(pointshape), linetype = from_theme(bordertype),
      linewidth = from_theme(borderwidth),
      width = 0.9
	),
    pattern_aesthetics
  ),
)
