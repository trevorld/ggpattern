suppressPackageStartupMessages({
  library("dplyr")
  library("glue")
})

template_cont_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for continuous pattern aesthetics
#'
#' @param name,breaks,labels,limits,trans,guide,...,transform See
#'        \\code{{ggplot2}} documentation for more information on scales.
#' @param range A numeric vector of length 2 of the minimum and maximum output values.
#'        If `NULL` (and `palette` is `NULL`) uses the `palette.<aesthetic>.<type>` theme element
#'        (see [pattern_palettes]) else the default palette (see the \"Default palettes\" section).
#' @param palette A palette function e.g. [scales::pal_rescale()] for a continuous scale
#'        or a `function(n)` returning `n` values for a discrete scale.
#'        Takes precedence over `range`.
#'        For a vector of values use `range` or the `scale_pattern_*_manual()` scales.
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
"

template_discrete_roxygen_first <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scales for discrete pattern aesthetics
#'
#' @param choices vector of values to choose from.
#'        A continuous scale splits the range of its limits into `length(choices)` equal-width bins
#'        (a value on the edge between two bins goes in the lower bin)
#'        and gives each bin the next choice.
#'        If `NULL` (and `palette` is `NULL`) uses the `palette.<aesthetic>.<type>` theme element
#'        (see [pattern_palettes]) else the default palette (see the \"Default palettes\" section).
#' @param palette A palette function, or for a discrete scale a character vector of values
#'        (used in order, see [scales::pal_manual()]).
#'        Takes precedence over `choices`.
#'        For a vector of values for a continuous scale use `choices`.
#' @param name,breaks,labels,limits,trans,guide,...,transform,na.value See
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
"

# Roxygen \"Default palettes\" section listing the default `range` or `choices` of each aesthetic
roxygen_default_palettes <- function(df, arg, name) {
  items <- ifelse(df$scale_default == "NULL",
                  glue("#' * `{df$aes_name}`: none (`{arg}`, `palette`, or a theme palette must be set)"),
                  glue("#' * `{df$aes_name}`: `{arg} = {df$scale_default}`"))
  if (arg == "choices") {
    # `scale_pattern_discrete()` and `scale_pattern_continuous()` are hand-written in 'R/scale-pattern.R'
    items <- c("#' * `pattern`: `choices = c('stripe', 'crosshatch', 'circle')`", items)
  }
  paste0(
    "#' @section Default palettes:\n",
    "#' If neither the `palette` nor `", arg, "` argument nor the `palette.<aesthetic>.<type>`\n",
    "#' theme element is set these scales use the following `", arg, "`:\n",
    "#'\n",
    paste0(items, collapse = "\n"), "\n",
    "#' @name ", name, "\n",
    "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n",
    "NULL\n\n"
  )
}

template_cont_var_cont_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_continuous <- function(name = waiver(),
                                        breaks = waiver(),
                                        labels = waiver(),
                                        limits = NULL,
                                        range = NULL,
                                        trans = deprecated(),
                                        guide = 'legend',
                                        ...,
                                        transform = 'identity',
                                        palette = NULL) {{

  if (lifecycle::is_present(trans)) {{
     lifecycle::deprecate_warn('1.1.1',
                               'scale_{aes_name}_continuous(trans)',
                               'scale_{aes_name}_continuous(transform)')
     transform <- trans
  }}

  ggplot2::continuous_scale(
    aesthetics = '{aes_name}',
    palette    = as_scale_palette(palette, range, scales::rescale_pal, 'continuous', '{aes_name}', 'range'),
    fallback.palette = {fallback_continuous},
    name       = name,
    breaks     = breaks,
    labels     = labels,
    limits     = limits,
    transform  = transform,
    guide      = guide,
    ...
  )
}}

"

template_discrete_var_cont_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_discrete <- function(..., range = NULL, palette = NULL, guide = 'legend') {{
  ggplot2::discrete_scale(
    aesthetics = '{aes_name}',
    palette    = as_scale_palette(palette, range, pal_seq_range, 'discrete', '{aes_name}', 'range'),
    fallback.palette = {fallback_discrete},
    guide      = guide,
    ...
  )
}}

"

template_cont_var_discrete_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_continuous <- function(name = waiver(),
                                        breaks = waiver(),
                                        labels = waiver(),
                                        limits = NULL,
                                        choices = NULL,
                                        trans = deprecated(),
                                        guide = 'legend',
                                        ...,
                                        transform = 'identity',
                                        palette = NULL) {{

  if (lifecycle::is_present(trans)) {{
     lifecycle::deprecate_warn('1.1.1',
                               'scale_{aes_name}_continuous(trans)',
                               'scale_{aes_name}_continuous(transform)')
     transform <- trans
  }}

  ggplot2::continuous_scale(
    aesthetics = '{aes_name}',
    palette    = as_scale_palette(palette, choices, pal_choices_continuous, 'continuous', '{aes_name}', 'choices'),
    fallback.palette = {fallback_continuous},
    name       = name,
    breaks     = breaks,
    labels     = labels,
    limits     = limits,
    transform  = transform,
    guide      = guide,
    ...)
}}


"

template_discrete_var_discrete_aes <- "
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_{aes_name}_discrete <- function(..., choices = NULL, palette = NULL, guide = 'legend') {{
  ggplot2::discrete_scale(
    aesthetics = '{aes_name}',
    palette    = as_scale_palette(palette, choices, pal_choices_discrete, 'discrete', '{aes_name}', 'choices'),
    fallback.palette = {fallback_discrete},
    guide      = guide,
    ...
  )
}}

"

source("data-raw/config.R")

ggpattern_scales_aes <- ggpattern_aes |>
  filter(is.na(ggplot_name)) |>
  mutate(
    arg = ifelse(aes_type == "continuous", "range", "choices"),
    no_default = scale_default == "NULL",
    fallback_continuous = ifelse(
      no_default,
      glue("pal_required('{aes_name}', 'continuous', '{arg}')"),
      ifelse(aes_type == "continuous",
             glue("scales::rescale_pal({scale_default})"),
             glue("pal_choices_continuous({scale_default})"))
    ),
    fallback_discrete = ifelse(
      no_default,
      glue("pal_required('{aes_name}', 'discrete', '{arg}')"),
      ifelse(aes_type == "continuous",
             glue("pal_seq_range({scale_default})"),
             glue("pal_choices_discrete({scale_default})"))
    )
  )

stopifnot(all(ggpattern_scales_aes$aes_type %in% c("continuous", "discrete")))

cat(template_cont_roxygen_first)
cat(roxygen_default_palettes(filter(ggpattern_scales_aes, aes_type == "continuous"),
                             "range", "scale_continuous"))
cat(template_discrete_roxygen_first)
cat(roxygen_default_palettes(filter(ggpattern_scales_aes, aes_type == "discrete"),
                             "choices", "scale_discrete"))

for (i in seq(nrow(ggpattern_scales_aes))) {
  this_aes <- ggpattern_scales_aes[i,]
  if (this_aes$aes_type == 'continuous') {
    cat(glue_data(this_aes, template_cont_var_cont_aes))
    cat(glue_data(this_aes, template_discrete_var_cont_aes))
  } else {
    cat(glue_data(this_aes, template_cont_var_discrete_aes))
    cat(glue_data(this_aes, template_discrete_var_discrete_aes))
  }
}
