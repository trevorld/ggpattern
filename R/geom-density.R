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
#' GeomDensityPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @import grid
#'
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
      size     = 0.5,
      linetype = 1,
      weight   = 1,
      alpha    = NA
    )
  )
)



if (FALSE) {
  library(ggplot2)

  ggplot(mtcars) +
    geom_density_pattern(aes(x=mpg, fill=as.factor(cyl), pattern = as.factor(cyl))) +
    theme_bw() +
    labs(title = "ggpattern::geom_density_pattern()")
}




