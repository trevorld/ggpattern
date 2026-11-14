#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPointPattern <- ggproto("GeomPointPattern", GeomPoint,
  # only the filled symbols (`shape` 21 to 25) have an area to fill with a pattern
  default_aes = defaults(aes(shape = 21), defaults(GeomPoint$default_aes, pattern_aesthetics)),

  setup_params = function(self, data, params) {
    check_r_version("geom_point_pattern")
    ggproto_parent(GeomPoint, self)$setup_params(data, params)
  },

  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    grob <- ggproto_parent(GeomPoint, self)$draw_panel(data, panel_params, coord, na.rm = na.rm)
    # only the filled symbols have an area (in row order): the other symbols are left alone
    # (each point's fill, pattern, and border are drawn in turn so overlapping points stack)
    is_filled <- is_filled_shape(data$shape)
    if (any(is_filled)) grob <- with_pattern_aes(grob, data[is_filled, , drop = FALSE])
    ggname("geom_point_pattern", grob)
  },

  draw_key = function(data, params, size) draw_key_point_pattern(data, params, size)
)

#' @rdname geom-docs
#' @export
geom_point_pattern <- make_constructor(GeomPointPattern)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname draw_key_polygon_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_point_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {
  key <- ggplot2::draw_key_point(data, params, size)
  if (!is_filled_shape(data$shape %||% 19)) return(key)
  # {ggplot2} enlarges a point's key (whose glyph has no size attributes) to fit the point
  # (see `ggplot2:::set_key_size()`) after drawing it so `size` may be too small
  point_size <- (data$size %||% 1.5) + (data$linewidth %||% 0)
  with_pattern_key(key, data, pmax(size, point_size), aspect_ratio)
}

# Whether point `shape`s are filled symbols (`pch` 21 to 25) i.e. have an area to fill with a pattern
is_filled_shape <- function(shape) {
  translate_shape_string(shape) %in% 21:25
}
