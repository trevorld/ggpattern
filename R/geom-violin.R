
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname geom_rect_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_violin_pattern <- function(mapping = NULL, data = NULL,
                                stat = "ydensity", position = "dodge",
                                ...,
                                draw_quantiles = NULL,
                                trim = TRUE,
                                scale = "area",
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomViolinPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomViolinPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomViolinPattern <- ggproto(
  "GeomViolinPattern", GeomViolin,

  draw_group = function(self, data, ..., draw_quantiles = NULL, flipped_aes = FALSE) {

    # Need to calculate aspect ratio here for compatibility with key drawing.
    args <- list(...)
    self$aspect_ratio <- get_aspect_ratio_from_context(args$coord, args$panel_params)

    data <- flip_data(data, flipped_aes)
    # Find the points for the line to go all the way around
    data <- transform(data,
                      xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x)
    )

    # Make sure it's sorted properly to draw the outline
    newdata <- rbind(
      transform(data, x = xminv)[order(data$y), ],
      transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
    )

    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    newdata <- flip_data(newdata, flipped_aes)

    # Draw quantiles if requested, so long as there is non-zero y range
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      if (!(all(draw_quantiles >= 0) && all(draw_quantiles <= 1))) {
        abort("`draw_quantiles must be between 0 and 1")
      }

      # Compute the quantile segments and combine with existing aesthetics
      quantiles <- create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[
        rep(1, nrow(quantiles)),
        setdiff(names(data), c("x", "y", "group")),
        drop = FALSE
      ]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      both <- both[!is.na(both$group), , drop = FALSE]
      both <- flip_data(both, flipped_aes)
      quantile_grob <- if (nrow(both) == 0) {
        zeroGrob()
      } else {
        GeomPath$draw_panel(both, ...)
      }

      ggname("geom_violin", grobTree(
        GeomPolygonPattern$draw_panel(newdata, ...),
        quantile_grob
      ))
    } else {
      ggname("geom_violin", GeomPolygonPattern$draw_panel(newdata, ...))
    }
  },

  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      weight   = 1,
      colour   = "grey20",
      fill     = "white",
      size     = 0.5,
      alpha    = NA,
      linetype = "solid"
    )
  ),


  aspect_ratio = 1,

  draw_key = function(self, ...) {
    draw_key_polygon_pattern(..., aspect_ratio = self$aspect_ratio)
  }
)



if (FALSE) {
  library(ggplot2)
  library(dplyr)

  ggplot(mtcars, aes(as.factor(cyl), mpg)) +
    geom_violin_pattern(aes(pattern = as.factor(cyl))) +
    theme_bw() +
    labs(title = "ggpattern::geom_violin_pattern()")
}
