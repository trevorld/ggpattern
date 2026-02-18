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
    data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(fill = fill_alpha(data$fill, data$alpha), group = data$group)

    whiskers <- data_frame0(
        x = c(data$x, data$x),
        xend = c(data$x, data$x),
        y = c(data$upper, data$lower),
        yend = c(data$ymax, data$ymin),
        colour = rep(whisker_gp$colour %||% data$colour, 2),
        linetype = rep(whisker_gp$linetype %||% data$linetype, 2),
        linewidth = rep(whisker_gp$linewidth %||% data$linewidth, 2),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- transform(
	  data,
	  y = middle,
	  ymax = upper,
	  ymin = lower,
      ynotchlower = ifelse(notch, notchlower, NA),
      ynotchupper = ifelse(notch, notchupper, NA),
      notchwidth = notchwidth
    )

    # Copy across all the pattern aesthetics
    for (varname in names(pattern_aesthetics)) {
      box[[varname]] <- data[[varname]]
    }

    box <- flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]]) >= 1) {
      outliers <- data_frame0(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier_gp$colour %||% data$colour[1],
        fill = outlier_gp$fill %||% data$fill[1],
        shape = outlier_gp$shape %||% data$shape[1] %||% 19,
        size = outlier_gp$size %||% data$size[1] %||% 1.5,
        stroke = outlier_gp$stroke %||% data$stroke[1] %||% 0.5,
        fill = NA,
        alpha = outlier_gp$alpha %||% data$alpha[1],
        .size = length(data$outliers[[1]])
      )
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    if (staplewidth != 0) {
      staples <- data_frame0(
        x    = rep((data$xmin - data$x) * staplewidth + data$x, 2),
        xend = rep((data$xmax - data$x) * staplewidth + data$x, 2),
        y    = c(data$ymax, data$ymin),
        yend = c(data$ymax, data$ymin),
        linetype = rep(staple_gp$linetype %||% data$linetype, 2),
        linewidth = rep(staple_gp$linewidth %||% data$linewidth, 2),
        colour = rep(staple_gp$colour %||% data$colour, 2),
        alpha = c(NA_real_, NA_real_),
        !!!common,
        .size = 2
      )
      staples <- flip_data(staples, flipped_aes)
      staple_grob <- GeomSegment$draw_panel(
        staples, panel_params, coord,
        lineend = lineend
      )
    } else {
      staple_grob <- NULL
    }

    ggname("geom_boxplot_pattern", grobTree(
      outliers_grob,
      staple_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord, lineend = lineend),
      GeomCrossbarPattern$draw_panel(
        box,
        fatten = fatten,
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin,
        flipped_aes = flipped_aes,
        middle_gp = median_gp,
        box_gp = box_gp
      )
    ))
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
