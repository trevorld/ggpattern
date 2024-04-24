#' @rdname geom-docs
#' @export
geom_boxplot_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "boxplot", position = "dodge2",
                                 ...,
                         outliers = TRUE,
                                 outlier.colour = NULL,
                                 outlier.color = NULL,
                                 outlier.fill = NULL,
                                 outlier.shape = 19,
                                 outlier.size = 1.5,
                                 outlier.stroke = 0.5,
                                 outlier.alpha = NULL,
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
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
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

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2, outlier.colour = NULL,
                        outlier.fill = NULL, outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5,
                        staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE) {
    data <- check_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      cli::cli_abort(c(
        "Can only draw one boxplot per group.",
        "i"= "Did you forget {.code aes(group = ...)}?"
      ))
    }

    common <- list(
      colour = data$colour,
      linewidth = data$linewidth,
      linetype = data$linetype,
      fill = fill_alpha(data$fill, data$alpha),
      group = data$group
    )

    whiskers <- data_frame0(
        x = c(data$x, data$x),
        xend = c(data$x, data$x),
        y = c(data$upper, data$lower),
        yend = c(data$ymax, data$ymin),
      alpha = c(NA_real_, NA_real_),
      !!!common,
      .size = 2
    )
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- data_frame0(
        xmin        = data$xmin,
        xmax        = data$xmax,
        ymin        = data$lower,
        y           = data$middle,
        ymax        = data$upper,
        ynotchlower = ifelse(notch, data$notchlower, NA),
        ynotchupper = ifelse(notch, data$notchupper, NA),
        notchwidth  = notchwidth,
      alpha = data$alpha,
      !!!common
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
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1],
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
        flipped_aes = flipped_aes
      )
    ))
  },

  draw_key = function(self, ...) draw_key_boxplot_pattern(...),

  default_aes = defaults(aes(weight = 1, colour = "grey20", fill = "white", size = NULL,
      alpha = NA, shape = 19, linetype = "solid", linewidth = 0.5),
    pattern_aesthetics
  ),

  rename_size = TRUE
)
