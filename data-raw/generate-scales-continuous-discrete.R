suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
})

template_cont_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for continuous pattern aesthetics
#'
#' @param name,breaks,labels,limits,range,trans,guide,... See
#'        \\code{{ggplot2}} documentation for more information on scales.
#'
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require('ggplot2')) {
#'
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c('a', 'b', 'c', 'd'),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level,
#'             pattern_density = outcome),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_density_continuous(range = c(0.1, 0.6)) +
#'       labs(
#'         title    = 'ggpattern::geom_col_pattern()',
#'         subtitle = 'pattern = \\\'stripe\\\''
#'       )
#'     plot(gg)
#'   }
#' @name scale_continuous
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL

"

template_cont_var_cont_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_continuous
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
#' @rdname scale_continuous
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

template_discrete_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for discrete pattern aesthetics
#'
#' @param choices vector of values to choose from.
#' @param name,breaks,labels,limits,trans,guide,... See
#'        \\code{{ggplot2}} documentation for more information on scales.
#'
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require('ggplot2')) {
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
#'        scale_pattern_type_discrete(choices = gridpattern::names_polygon_tiling) +
#'        theme_bw(18) +
#'        theme(legend.key.size = unit(2, 'cm')) +
#'        labs(
#'          title    = 'ggpattern::geom_density_pattern()',
#'          subtitle = 'pattern = \\\'polygon_tiling\\\''
#'        )
#'     plot(gg)
#'   }
#' @name scale_discrete
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NULL
"

template_cont_var_discrete_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
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
#' @rdname scale_discrete
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

cat(template_cont_roxygen_first)
cat(template_discrete_roxygen_first)

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









