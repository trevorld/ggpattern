#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_continuous <- function(name = waiver(),
                                        breaks = waiver(),
                                        labels = waiver(),
                                        limits = NULL,
                                        choices = NULL,
                                        trans = deprecated(),
                                        guide = 'legend',
                                        ...,
                                        transform = 'identity',
                                        na.value = 'none',
                                        palette = NULL) {

  if (lifecycle::is_present(trans)) {
     lifecycle::deprecate_warn('1.1.1',
                               'scale_pattern_continuous(trans)',
                               'scale_pattern_continuous(transform)')
     transform <- trans
  }

  ggplot2::continuous_scale(
    aesthetics = 'pattern',
    palette    = as_scale_palette(palette, choices, pal_choices_continuous, 'continuous', 'pattern', 'choices'),
    fallback.palette = pal_choices_continuous(c('stripe', 'crosshatch', 'circle')),
    name       = name,
    breaks     = breaks,
    labels     = labels,
    limits     = limits,
    transform  = transform,
    guide      = guide,
    na.value   = na.value,
    ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_discrete <- function(..., choices = NULL, palette = NULL,
                                   guide = 'legend', na.value = "none") {
  ggplot2::discrete_scale(
    aesthetics = 'pattern',
    palette    = as_scale_palette(palette, choices, pal_choices_discrete, 'discrete', 'pattern', 'choices'),
    fallback.palette = pal_choices_discrete(c('stripe', 'crosshatch', 'circle')),
    guide = guide,
    na.value = na.value,
    ...
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_identity
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_identity <- function(..., guide = 'none') {
  discrete_scale(
    aesthetics = 'pattern',
    palette    = identity_pal(),
    ...,
    guide      = guide,
    super      = ScaleDiscreteIdentity
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_manual
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_manual <- function(..., values, breaks = waiver(), na.value = "none") {
  manual_scale('pattern', values, breaks, na.value = na.value, ...)
}
