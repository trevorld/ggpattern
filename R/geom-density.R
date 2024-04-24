#' @rdname geom-docs
#' @export
geom_density_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "density", position = "identity",
                                 ...,
                                 na.rm = FALSE,
                                 orientation = NA,
                                 show.legend = NA,
                         inherit.aes = TRUE,
                         outline.type = "upper") {
  outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensityPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-ribbon.R
GeomDensityPattern <- ggproto("GeomDensityPattern", GeomAreaPattern,
  default_aes = defaults(
    aes(fill = NA, weight = 1, colour = "black", alpha = NA),
    defaults(GeomArea$default_aes, pattern_aesthetics)
  )
)
