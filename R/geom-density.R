#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_density_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "density", position = "identity",
                                 ...,
                                 na.rm = FALSE,
                                 orientation = NA,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @include geom-ribbon.R
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomDensityPattern <- ggproto(
  "GeomDensityPattern", GeomAreaPattern,
  default_aes = augment_aes(
    pattern_aesthetics,
    ggplot2::aes(
      colour   = 'black',
      fill     = "NA",
      linewidth= 0.5,
      linetype = 1,
      weight   = 1,
      alpha    = NA
    )
  ),
  rename_size = TRUE
)
