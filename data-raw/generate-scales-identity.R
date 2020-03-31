
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
    scale_name = 'identity',
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
    scale_name = 'identity',
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

# sink("R/scale-pattern-identity.R")

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

# sink()













