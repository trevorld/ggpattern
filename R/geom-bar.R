#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ggplot2 geoms with support for pattern fills
#'
#' All geoms in this package are identical to their counterparts in ggplot2 except
#' that they can be filled with patterns.
#'
#' @section Pattern Arguments:
#'
#' Not all arguments apply to all patterns.
#'
#' \describe{
#' \item{\strong{\code{pattern}}}{Pattern name string e.g. 'stripe' (default), 'crosshatch', 'point', 'circle', 'none'}
#' \item{\strong{\code{pattern_colour       }}}{ Colour used for strokes and points. default: 'black'}
#' \item{\strong{\code{pattern_fill         }}}{ Fill colour. default: 'black'}
#' \item{\strong{\code{pattern_angle        }}}{ Orientation of the pattern in degrees. default: 45}
#' \item{\strong{\code{pattern_density      }}}{ Approximate fill fraction of the pattern. Usually in range \[0, 1], but can be higher. default: 0.2}
#' \item{\strong{\code{pattern_spacing      }}}{ Spacing of the pattern as a fraction of the plot size. default: 0.05}
#' \item{\strong{\code{pattern_xoffset,pattern_yoffset}}}{Offset the origin of the pattern. Range \[0, 1]. default: 0.  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#' \item{\strong{\code{pattern_alpha        }}}{ Alpha transparency for pattern. default: 1}
#' \item{\strong{\code{pattern_linetype     }}}{ Stroke linetype. default: 1}
#' \item{\strong{\code{pattern_size         }}}{ Stroke line width. default: 1}
#' \item{\strong{\code{pattern_option_1     }}}{ Generic User value }
#' }
#'
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::geom_boxplot
#' @inheritParams ggplot2::geom_crossbar
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams ggplot2::geom_map
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams ggplot2::geom_violin
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_bar_pattern <- function(mapping = NULL, data = NULL,
                             stat = "count", position = "stack",
                             ...,
                             width = NULL,
                             binwidth = NULL,
                             na.rm = FALSE,
                             orientation = NA,
                             show.legend = NA,
                             inherit.aes = TRUE) {

  if (!is.null(binwidth)) {
    warn("`geom_bar_pattern()` no longer has a `binwidth` parameter. Please use `geom_histogram()` instead.")
    return(geom_histogram(mapping = mapping, data = data,
                          position = position, width = width, binwidth = binwidth, ...,
                          na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomBarPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#' @include geom-rect.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomBarPattern <- ggproto(
  "GeomBarPattern", GeomRectPattern,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, range_is_orthogonal = FALSE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data <- transform(data,
                      ymin = pmin(y, 0), ymax = pmax(y, 0),
                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    ggplot2::flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRectPattern, self)$draw_panel(data, panel_params, coord)
  }
)
