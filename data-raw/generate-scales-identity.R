suppressPackageStartupMessages({
  library(glue)
  library(dplyr)
})

template_identity_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Use values without scaling
#'
#' @param ...,guide See \\code{{ggplot2}} for documentation on identity scales.
#'        e.g. \\code{{ggplot2::scale_alpha_identity()}}
#'
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require('ggplot2')) {{
#'     df <- data.frame(outcome = c(2.3, 1.9, 3.2, 1),
#'                      pattern_type = sample(gridpattern::names_polygon_tiling, 4))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(pattern_type, outcome, pattern_fill = pattern_type,
#'             pattern_type = pattern_type),
#'         colour  = 'black',
#'         pattern = 'polygon_tiling',
#'         pattern_key_scale_factor = 1.2
#'        ) +
#'        scale_pattern_type_identity() +
#'        theme_bw(18) +
#'        theme(legend.position = 'none') +
#'        labs(
#'          x        = 'level',
#'          title    = 'ggpattern::geom_col_pattern()',
#'          subtitle = 'pattern = \\\'polygon_tiling\\\''
#'        )
#'     plot(gg)
#'   }}
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"

template_identity_roxygen_other <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_{aes_name}_identity
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"

template_identity_discrete <- "
scale_{aes_name}_identity <- function(..., guide = 'none') {{
  discrete_scale(
    aesthetics = '{aes_name}',
    palette    = identity_pal(),
    ...,
    guide      = guide,
    super      = ScaleDiscreteIdentity
  )
}}

"

template_identity_continuous <- "
scale_{aes_name}_identity <- function(..., guide = 'none') {{
  continuous_scale(
    aesthetics = '{aes_name}',
    palette    = identity_pal(),
    ...,
    guide      = guide,
    super      = ScaleContinuousIdentity
  )
}}

"

source("data-raw/config.R")

all_aes <- ggpattern_aes

first_aes <- all_aes[1,]

for (i in seq(nrow(all_aes))) {
  this_aes <- all_aes[i,]
  if (i == 1) {
    cat(glue_data(this_aes, template_identity_roxygen_first))
  } else {
    cat(glue_data(first_aes, template_identity_roxygen_other))
  }

  if (this_aes$aes_type == 'continuous') {
    cat(glue_data(this_aes, template_identity_continuous))
  } else if (this_aes$aes_type == 'discrete') {
    cat(glue_data(this_aes, template_identity_discrete))
  } else {
    stop("no such type: ", this_aes$aes_type)
  }

}
