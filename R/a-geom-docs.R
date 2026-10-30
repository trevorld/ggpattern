#' ggplot2 geoms with support for pattern fills
#'
#' All geoms in this package are identical to their counterparts in ggplot2 except
#' that they can be filled with patterns.
#'
#' @section Pattern Arguments:
#'
#' Not all arguments apply to all patterns.
#'
#' Most pattern aesthetics default to the pattern's own default
#' (see the documentation of the corresponding `gridpattern::grid.pattern_<name>()` function)
#' which for most patterns is `{gridpattern}`'s generic default (listed below as "generic").
#' Internally these unset aesthetics have the value `NaN`, which `{gridpattern}` treats as unset
#' (whereas `NA` may be meaningful e.g. a transparent colour or a "pch" shape that draws nothing).
#'
#' \describe{
#' \item{\strong{`pattern`}}{Pattern name string e.g. 'stripe' (default), 'crosshatch', 'point', 'circle', 'none'}
#' \item{\strong{`pattern_alpha`}}{ Alpha transparency for pattern. default: pattern default (generic: `NA` i.e. keep the colours' alpha) }
#' \item{\strong{`pattern_angle`}}{ Orientation of the pattern in degrees. default: pattern default (generic: 30) }
#' \item{\strong{`pattern_aspect_ratio`}}{ Aspect ratio adjustment. default: the aspect ratio of the plot panel / legend key }
#' \item{\strong{`pattern_colour`}}{ Colour used for strokes and points. default: pattern default (generic: 'grey20') }
#' \item{\strong{`pattern_density`}}{ Approximate fill fraction of the pattern. Usually in range \[0, 1], but can be higher. default: pattern default (generic: 0.2) }
#' \item{\strong{`pattern_filename`}}{ Image filename/URL. default: pattern default (generic: '') }
#' \item{\strong{`pattern_fill`}}{ Fill colour (or [grid::pattern()]/gradient fill). default: pattern default (generic: 'grey80') }
#' \item{\strong{`pattern_fill2`}}{ Second fill colour (or [grid::pattern()]/gradient fill). default: pattern default (generic: '#4169E1') }
#' \item{\strong{`pattern_filter`}}{ (Image scaling) filter. default: pattern default (generic: 'lanczos') }
#' \item{\strong{`pattern_frequency`}}{ Frequency. default: pattern default (generic: 0.1) }
#' \item{\strong{`pattern_gravity`}}{ Image placement. default: pattern default (generic: 'center') }
#' \item{\strong{`pattern_grid`}}{ Pattern grid type. default: pattern default (generic: 'square') }
#' \item{\strong{`pattern_key_scale_factor`}}{ Zoom factor for the pattern in the legend key (multiplies the size of relative i.e. 'snpc' lengths relative to the key as well as the spacing and offsets of absolute lengths). default: 1 }
#' \item{\strong{`pattern_linetype`}}{ Stroke linetype. default: pattern default (generic: 1) }
#' \item{\strong{`pattern_option_1`}}{ Generic user value for custom patterns. default: pattern default (generic: 0) }
#' \item{\strong{`pattern_option_2`}}{ Generic user value for custom patterns. default: pattern default (generic: 0) }
#' \item{\strong{`pattern_option_3`}}{ Generic user value for custom patterns. default: pattern default (generic: 0) }
#' \item{\strong{`pattern_option_4`}}{ Generic user value for custom patterns. default: pattern default (generic: 0) }
#' \item{\strong{`pattern_option_5`}}{ Generic user value for custom patterns. default: pattern default (generic: 0) }
#' \item{\strong{`pattern_orientation`}}{ 'vertical', 'horizontal', or 'radial'. default: pattern default (generic: 'vertical') }
#' \item{\strong{`pattern_phase`}}{ Phase (generic user value for custom patterns). default: pattern default (generic: 0) }
#' \item{\strong{`pattern_res`}}{ Pattern resolution (pixels per inch). default: `getOption("ggpattern_res")` if set else slightly above the device resolution (72 ppi for vector devices such as PDF/SVG); legend keys use a higher resolution so that pixel-based patterns look like they do in the plot }
#' \item{\strong{`pattern_rot`}}{ Rotation angle (shape within pattern). default: pattern default (generic: 0) }
#' \item{\strong{`pattern_scale`}}{ Scale. default: pattern default (generic: 1) }
#' \item{\strong{`pattern_shape`}}{ Plotting shape. default: pattern default (generic: 1) }
#' \item{\strong{`pattern_size`}}{ Stroke line width (or fontsize). default: pattern default (generic: 0.5) }
#' \item{\strong{`pattern_spacing`}}{ Spacing of the pattern as a fraction of the plot size. default: pattern default (generic: 0.05) }
#' \item{\strong{`pattern_type`}}{ Generic control option. default: pattern default (generic: `NA`) }
#' \item{\strong{`pattern_subtype`}}{ Generic control option. default: pattern default (generic: `NA`) }
#' \item{\strong{`pattern_xoffset`}}{Offset the origin of the pattern. Range \[0, 1]. default: pattern default (generic: 0).  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#' \item{\strong{`pattern_yoffset`}}{Offset the origin of the pattern. Range \[0, 1]. default: pattern default (generic: 0).  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#' }
#' @inheritParams ggplot2::geom_bar
#' @inheritParams ggplot2::geom_boxplot
#' @inheritParams ggplot2::geom_crossbar
#' @inheritParams ggplot2::geom_histogram
#' @inheritParams ggplot2::geom_map
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_ribbon
#' @inheritParams ggplot2::geom_tile
#' @inheritParams ggplot2::geom_violin
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
#'                                   "pythagorean")) +
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
#' @name geom-docs
NULL
