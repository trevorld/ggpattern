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
#' \item{\strong{`pattern`}}{Pattern name string e.g. 'stripe' (default), 'crosshatch', 'point', 'circle', 'none'}
#' \item{\strong{`pattern_alpha`}}{ Alpha transparency for pattern. default: 1}
#' \item{\strong{`pattern_angle`}}{ Orientation of the pattern in degrees. default: 30}
#' \item{\strong{`pattern_aspect_ratio`}}{ Aspect ratio adjustment. }
#' \item{\strong{`pattern_colour`}}{ Colour used for strokes and points. default: 'black'}
#' \item{\strong{`pattern_density`}}{ Approximate fill fraction of the pattern. Usually in range \[0, 1], but can be higher. default: 0.2}
#' \item{\strong{`pattern_filename`}}{ Image filename/URL. }
#' \item{\strong{`pattern_fill`}}{ Fill colour. default: 'grey80'}
#' \item{\strong{`pattern_fill2`}}{ Second fill colour. default: '#4169E1'}
#' \item{\strong{`pattern_filter`}}{ (Image scaling) filter. default: 'lanczos' }
#' \item{\strong{`pattern_frequency`}}{ Frequency. default: 0.1 }
#' \item{\strong{`pattern_gravity`}}{ Image placement. default: 'center' }
#' \item{\strong{`pattern_grid`}}{ Pattern grid type. default: 'square' }
#' \item{\strong{`pattern_key_scale_factor`}}{ Scale factor for pattern in legend. default: 1 }
#' \item{\strong{`pattern_linetype`}}{ Stroke linetype. default: 1}
#' \item{\strong{`pattern_option_1`}}{ Generic user value for custom patterns. }
#' \item{\strong{`pattern_option_2`}}{ Generic user value for custom patterns. }
#' \item{\strong{`pattern_option_3`}}{ Generic user value for custom patterns. }
#' \item{\strong{`pattern_option_4`}}{ Generic user value for custom patterns. }
#' \item{\strong{`pattern_option_5`}}{ Generic user value for custom patterns. }
#' \item{\strong{`pattern_orientation`}}{ 'vertical', 'horizontal', or 'radial'. default: 'vertical' }
#' \item{\strong{`pattern_res`}}{ Pattern resolution (pixels per inch). }
#' \item{\strong{`pattern_rot`}}{ Rotation angle (shape within pattern). default: 0 }
#' \item{\strong{`pattern_scale`}}{ Scale. default: 1}
#' \item{\strong{`pattern_shape`}}{ Plotting shape. default: 1}
#' \item{\strong{`pattern_size`}}{ Stroke line width. default: 1}
#' \item{\strong{`pattern_spacing`}}{ Spacing of the pattern as a fraction of the plot size. default: 0.05}
#' \item{\strong{`pattern_type`}}{ Generic control option }
#' \item{\strong{`pattern_subtype`}}{ Generic control option }
#' \item{\strong{`pattern_xoffset`}}{Offset the origin of the pattern. Range \[0, 1]. default: 0.  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#' \item{\strong{`pattern_yoffset`}}{Offset the origin of the pattern. Range \[0, 1]. default: 0.  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
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
#' @examples
#'   if (require("ggplot2")) {
#'
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'stripe'"
#'       )
#'     plot(gg)
#'
#'     # 'pch' pattern example
#'     gg <- ggplot(mtcars, aes(as.factor(cyl), mpg)) +
#'       geom_violin_pattern(aes(fill = as.factor(cyl),
#'                               pattern_shape = as.factor(cyl)),
#'         pattern = 'pch',
#'         pattern_density = 0.3,
#'         pattern_angle = 0,
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       labs(
#'         title    = "ggpattern::geom_violin_pattern()",
#'         subtitle = "pattern = 'pch'"
#'       )
#'     plot(gg)
#'
#'     # 'polygon_tiling' pattern example
#'     gg <- ggplot(mtcars) +
#'        geom_density_pattern(
#'          aes(
#'            x            = mpg,
#'            pattern_fill = as.factor(cyl),
#'            pattern_type = as.factor(cyl)
#'          ),
#'          pattern = 'polygon_tiling',
#'          pattern_key_scale_factor = 1.2
#'        ) +
#'        scale_pattern_type_manual(values = c("hexagonal", "rhombille",
#'                                   "pythagorean", "rhombitrihexagonal")) +
#'        theme_bw(18) +
#'        theme(legend.key.size = unit(2, 'cm')) +
#'        labs(
#'          title    = "ggpattern::geom_density_pattern()",
#'          subtitle = "pattern = 'polygon_tiling'"
#'        )
#'     plot(gg)
#'   }
#'
#' @return A [ggplot2::Geom] object.
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
#' Geom ggproto objects
#'
#' Geom ggproto objects that could be extended to create a new geom.
#'
#' @seealso [ggplot2::Geom]
#'
#' @name ggpattern-ggproto
#' @format NULL
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
