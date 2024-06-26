% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-pattern-auto.R, R/scale-pattern.R,
%   R/zxx.r
\name{scale_pattern_manual}
\alias{scale_pattern_manual}
\alias{scale_pattern_type_manual}
\alias{scale_pattern_subtype_manual}
\alias{scale_pattern_angle_manual}
\alias{scale_pattern_density_manual}
\alias{scale_pattern_spacing_manual}
\alias{scale_pattern_xoffset_manual}
\alias{scale_pattern_yoffset_manual}
\alias{scale_pattern_alpha_manual}
\alias{scale_pattern_linetype_manual}
\alias{scale_pattern_size_manual}
\alias{scale_pattern_shape_manual}
\alias{scale_pattern_colour_manual}
\alias{scale_pattern_fill_manual}
\alias{scale_pattern_fill2_manual}
\alias{scale_pattern_aspect_ratio_manual}
\alias{scale_pattern_key_scale_factor_manual}
\alias{scale_pattern_filename_manual}
\alias{scale_pattern_filter_manual}
\alias{scale_pattern_gravity_manual}
\alias{scale_pattern_scale_manual}
\alias{scale_pattern_orientation_manual}
\alias{scale_pattern_phase_manual}
\alias{scale_pattern_frequency_manual}
\alias{scale_pattern_grid_manual}
\alias{scale_pattern_res_manual}
\alias{scale_pattern_rot_manual}
\alias{scale_pattern_units_manual}
\alias{scale_pattern_color_manual}
\title{Create your own discrete scale}
\usage{
scale_pattern_type_manual(..., values, breaks = waiver())

scale_pattern_subtype_manual(..., values, breaks = waiver())

scale_pattern_angle_manual(..., values, breaks = waiver())

scale_pattern_density_manual(..., values, breaks = waiver())

scale_pattern_spacing_manual(..., values, breaks = waiver())

scale_pattern_xoffset_manual(..., values, breaks = waiver())

scale_pattern_yoffset_manual(..., values, breaks = waiver())

scale_pattern_alpha_manual(..., values, breaks = waiver())

scale_pattern_linetype_manual(..., values, breaks = waiver())

scale_pattern_size_manual(..., values, breaks = waiver())

scale_pattern_shape_manual(..., values, breaks = waiver())

scale_pattern_colour_manual(..., values, breaks = waiver())

scale_pattern_fill_manual(..., values, breaks = waiver())

scale_pattern_fill2_manual(..., values, breaks = waiver())

scale_pattern_aspect_ratio_manual(..., values, breaks = waiver())

scale_pattern_key_scale_factor_manual(..., values, breaks = waiver())

scale_pattern_filename_manual(..., values, breaks = waiver())

scale_pattern_filter_manual(..., values, breaks = waiver())

scale_pattern_gravity_manual(..., values, breaks = waiver())

scale_pattern_scale_manual(..., values, breaks = waiver())

scale_pattern_orientation_manual(..., values, breaks = waiver())

scale_pattern_phase_manual(..., values, breaks = waiver())

scale_pattern_frequency_manual(..., values, breaks = waiver())

scale_pattern_grid_manual(..., values, breaks = waiver())

scale_pattern_res_manual(..., values, breaks = waiver())

scale_pattern_rot_manual(..., values, breaks = waiver())

scale_pattern_units_manual(..., values, breaks = waiver())

scale_pattern_manual(..., values, breaks = waiver(), na.value = "none")
}
\arguments{
\item{..., values, breaks, na.value}{See \code{ggplot2} for documentation on manual scales.
e.g. \code{ggplot2::scale_colour_manual()}}
}
\value{
A \link[ggplot2:ggplot2-ggproto]{ggplot2::Scale} object.
}
\description{
Create your own discrete scale
}
\examples{
  if (require('ggplot2')) {
    gg <- ggplot(mtcars) +
       geom_density_pattern(
         aes(
           x            = mpg,
           pattern_fill = as.factor(cyl),
           pattern_type = as.factor(cyl)
         ),
         pattern = 'polygon_tiling',
         pattern_key_scale_factor = 1.2
       ) +
       scale_pattern_type_manual(values = c('hexagonal', 'rhombille',
                                  'pythagorean')) +
       theme_bw(18) +
       theme(legend.key.size = unit(2, 'cm')) +
       labs(
         title    = 'ggpattern::geom_density_pattern()',
         subtitle = 'pattern = \'polygon_tiling\''
       )
    plot(gg)
  }
}
