
suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
})



template_cont_var_cont_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for {aes_name}
#'
#' @param name,breaks,labels,limits,range,trans,guide,... See
#'        \\code{{ggplot2}} documentation for more information on scales.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_continuous <- function(name   = waiver(),
                                        breaks = waiver(),
                                        labels = waiver(),
                                        limits = NULL,
                                        range  = {scale_default},
                                        trans  = 'identity',
                                        guide  = 'legend') {{


  if (is.null(range)) {{
     abort('scale_{aes_name}_continuous(): must specify \"range\" argument')
  }}

  ggplot2::continuous_scale(
    aesthetics = '{aes_name}',
    scale_name = '{aes_name}',
    palette    = scales::rescale_pal(range),
    name       = name,
    breaks     = breaks,
    labels     = labels,
    limits     = limits,
    trans      = trans,
    guide      = guide
  )
}}


"


template_discrete_var_cont_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_{aes_name}_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_discrete <- function(..., range = {scale_default}) {{
  force(range)

  if (is.null(range)) {{
     abort('scale_{aes_name}_discrete(): must specify \"range\" argument')
  }}

  ggplot2::discrete_scale(
    aesthetics = '{aes_name}',
    scale_name = '{aes_name}',
    palette    = function(n) seq(range[1], range[2], length.out = n),
    guide      = 'legend',
    ...
  )
}}


"


template_cont_var_discrete_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for {aes_name}
#'
#' @param choices vector of values to choose from.
#' @param name,breaks,labels,limits,trans,guide,... See
#'        \\code{{ggplot2}} documentation for more information on scales.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                     limits = NULL, choices = {scale_default},
                                     trans = 'identity', guide = 'legend') {{


  if (is.null(choices)) {{
     abort('scale_{aes_name}_continuous(): must specify \"choices\" argument')
  }}

  ggplot2::continuous_scale(
    aesthetics = '{aes_name}',
    scale_name = '{aes_name}',
    palette    = function(x) choices[as.integer(x * (length(choices) - 1) + 1)],
    name       = name,
    breaks     = breaks,
    labels     = labels,
    limits     = limits,
    trans      = trans,
    guide      = guide)
}}


"

template_discrete_var_discrete_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_{aes_name}_continuous
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_discrete <- function(..., choices = {scale_default}, guide = 'legend') {{
  force(range)

  if (is.null(choices)) {{
     abort('scale_{aes_name}_discrete(): must specify \"choices\" argument')
  }}

  ggplot2::discrete_scale(
    aesthetics = '{aes_name}',
    scale_name = '{aes_name}',
    palette    = function(n) {{
      idx <- cut(seq(n), length(choices), labels = FALSE, include.lowest = TRUE)
      choices[idx]
    }},
    guide = guide,
    ...
  )
}}


"



source("data-raw/config.R")

# sink("R/scale-pattern.R")

for (i in seq(nrow(ggpattern_aes))) {
  this_aes <- ggpattern_aes[i,]
  if (!is.na(this_aes$ggplot_name)) {
    next
  }

  if (this_aes$aes_type == 'continuous') {
    cat(glue_data(this_aes, template_cont_var_cont_aes))
    cat(glue_data(this_aes, template_discrete_var_cont_aes))
  } else if (this_aes$aes_type == 'discrete') {
    cat(glue_data(this_aes, template_cont_var_discrete_aes))
    cat(glue_data(this_aes, template_discrete_var_discrete_aes))
  } else {
    stop("No such aes_type: ", this_aes$aes_type)
  }
}

# sink()









