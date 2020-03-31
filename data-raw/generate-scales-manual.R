
suppressPackageStartupMessages({
  library(glue)
  library(dplyr)
})


template_manual_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create your own discrete scale
#'
#' @param ...,values,breaks See \\code{{ggplot2}} for documentation on manual scales.
#'        e.g. \\code{{ggplot2::scale_colour_manual()}}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"


template_manual_roxygen_other <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_{aes_name}_manual
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

# sink("R/scale-pattern-manual.R")

for (i in seq(nrow(all_aes))) {
  this_aes <- all_aes[i,]
  if (i == 1) {
    cat(glue_data(this_aes, template_manual_roxygen_first))
  } else {
    cat(glue_data(first_aes, template_manual_roxygen_other))
  }

  cat(glue_data(this_aes, template_manual))

}

# sink()













