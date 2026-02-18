#' Geom ggproto objects
#'
#' Geom ggproto objects that could be extended to create a new geom.
#'
#' @seealso [ggplot2::Geom]
#'
#' @name ggpattern-ggproto
NULL

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-rect.R
GeomBarPattern <- ggproto(
  "GeomBarPattern", GeomRectPattern,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  default_aes = aes(!!!GeomRectPattern$default_aes, width = 0.9),

  setup_params = GeomBar$setup_params,

  extra_params = c("just", "na.rm", "orientation"),

  setup_data = GeomBar$setup_data,

  rename_size = FALSE
)


#' @rdname geom-docs
#' @export
geom_bar_pattern <- make_constructor(
  GeomBarPattern,
  stat = "count", position = "stack", just = 0.5
)
