suppressPackageStartupMessages({
  library(glue)
  library(dplyr)
})

template_manual_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create your own discrete scale
#'
#' @param ...,values,breaks,na.value See \\code{{ggplot2}} for documentation on manual scales.
#'        e.g. \\code{{ggplot2::scale_colour_manual()}}
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require('ggplot2')) {{
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
#'        scale_pattern_type_manual(values = c('hexagonal', 'rhombille',
#'                                   'pythagorean')) +
#'        theme_bw(18) +
#'        theme(legend.key.size = unit(2, 'cm')) +
#'        labs(
#'          title    = 'ggpattern::geom_density_pattern()',
#'          subtitle = 'pattern = \\\'polygon_tiling\\\''
#'        )
#'     plot(gg)
#'   }}
#' @name scale_pattern_manual
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL

"

template_manual_roxygen_other <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_manual
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"

template_manual <- "
scale_{aes_name}_manual <- function(..., values, breaks = waiver()) {{
  manual_scale('{aes_name}', values, breaks, ...)
}}

"

source("data-raw/config.R")

all_aes <- ggpattern_aes

first_aes <- all_aes[1,]

for (i in seq(nrow(all_aes))) {
  this_aes <- all_aes[i,]
  if (i == 1) {
    cat(glue_data(this_aes, template_manual_roxygen_first))
    cat(glue_data(first_aes, template_manual_roxygen_other))
  } else {
    cat(glue_data(first_aes, template_manual_roxygen_other))
  }

  cat(glue_data(this_aes, template_manual))
}
