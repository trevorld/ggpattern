#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_discrete
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_continuous <- function(name = waiver(),
                                        breaks = waiver(),
                                        labels = waiver(),
                                        limits = NULL,
                                        choices = c('stripe', 'crosshatch', 'circle'),
                                        trans = deprecated(),
                                        guide = 'legend',
                                        ...,
                                        transform = 'identity',
                                        na.value = 'none') {

  if (is.null(choices)) {
     abort('scale_pattern_continuous(): must specify "choices" argument')
  }
  if (lifecycle::is_present(trans)) {
     lifecycle::deprecate_warn('1.1.1',
                               'scale_pattern_continuous(trans)',
                               'scale_pattern_continuous(transform)')
     transform <- trans
  }

  ggplot2::continuous_scale(
    aesthetics = 'pattern',
    palette    = function(x) choices[as.integer(x * (length(choices) - 1) + 1)],
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
#' @importFrom utils head
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_discrete <- function(..., choices = c('stripe', 'crosshatch', 'circle'),
                                   guide = 'legend', na.value = "none") {
  force(range)

  if (is.null(choices)) {
     abort('scale_pattern_discrete(): must specify "choices" argument')
  }

  ggplot2::discrete_scale(
    aesthetics = 'pattern',
    palette    = function(n) {
      idx <- cut(seq(n), length(choices), labels = FALSE, include.lowest = TRUE)
      choices[idx]
    },
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
