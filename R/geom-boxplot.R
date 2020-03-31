

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname geom_rect_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_boxplot_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "boxplot", position = "dodge2",
                                 ...,
                                 outlier.colour = NULL,
                                 outlier.color = NULL,
                                 outlier.fill = NULL,
                                 outlier.shape = 19,
                                 outlier.size = 1.5,
                                 outlier.stroke = 0.5,
                                 outlier.alpha = NULL,
                                 notch = FALSE,
                                 notchwidth = 0.5,
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
      warn("Can't preserve total widths when varwidth = TRUE.")
      position$preserve <- "single"
    }
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplotPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomBoxplotPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomBoxplotPattern <- ggproto(
  "GeomBoxplotPattern", GeomBoxplot,

  draw_group = function(self, data, panel_params, coord, fatten = 2,
                        outlier.colour = NULL, outlier.fill = NULL,
                        outlier.shape = 19,
                        outlier.size = 1.5, outlier.stroke = 0.5,
                        outlier.alpha = NULL,
                        notch = FALSE, notchwidth = 0.5, varwidth = FALSE, flipped_aes = FALSE) {
    data <- flip_data(data, flipped_aes)
    # this may occur when using geom_boxplot(stat = "identity")
    if (nrow(data) != 1) {
      abort("Can't draw more than one boxplot per group. Did you forget aes(group = ...)?")
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Hack needed so that legend/key drawing knows something about sizing
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    self$aspect_ratio <- get_aspect_ratio_from_context(coord, panel_params)

    common <- list(
      colour = data$colour,
      size = data$size,
      linetype = data$linetype,
      fill = alpha(data$fill, data$alpha),
      group = data$group
    )

    whiskers <- new_data_frame(c(
      list(
        x = c(data$x, data$x),
        xend = c(data$x, data$x),
        y = c(data$upper, data$lower),
        yend = c(data$ymax, data$ymin),
        alpha = c(NA_real_, NA_real_)
      ),
      common
    ), n = 2)
    whiskers <- flip_data(whiskers, flipped_aes)

    box <- new_data_frame(c(
      list(
        xmin        = data$xmin,
        xmax        = data$xmax,
        ymin        = data$lower,
        y           = data$middle,
        ymax        = data$upper,
        ynotchlower = ifelse(notch, data$notchlower, NA),
        ynotchupper = ifelse(notch, data$notchupper, NA),
        notchwidth  = notchwidth,
        alpha       = data$alpha
      ),
      common
    ))


    # Copy across all the pattern aesthetics
    for (varname in names(pattern_aesthetics)) {
      box[[varname]] <- data[[varname]]
    }


    box <- flip_data(box, flipped_aes)

    if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
      outliers <- new_data_frame(list(
        y = data$outliers[[1]],
        x = data$x[1],
        colour = outlier.colour %||% data$colour[1],
        fill = outlier.fill %||% data$fill[1],
        shape = outlier.shape %||% data$shape[1],
        size = outlier.size %||% data$size[1],
        stroke = outlier.stroke %||% data$stroke[1],
        fill = NA,
        alpha = outlier.alpha %||% data$alpha[1]
      ), n = length(data$outliers[[1]]))
      outliers <- flip_data(outliers, flipped_aes)

      outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
    } else {
      outliers_grob <- NULL
    }

    ggname("geom_boxplot", grobTree(
      outliers_grob,
      GeomSegment$draw_panel(whiskers, panel_params, coord),
      GeomCrossbarPattern$draw_panel(box, fatten = fatten, panel_params, coord, flipped_aes = flipped_aes),
      grid::nullGrob()
    ))
  },

  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      weight   = 1,
      colour   = "grey20",
      fill     = "white",
      size     = 0.5,
      alpha    = NA,
      shape    = 19,
      linetype = "solid"
    )
  ),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hack needed so that legend/key drawing knows something about sizing
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  aspect_ratio = 1,

  draw_key = function(self, ...) {
    draw_key_boxplot_pattern(..., aspect_ratio = self$aspect_ratio)
  }
)

