

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @usage NULL
#' @rdname geom_rect_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_bar_pattern <- function(mapping = NULL, data = NULL,
                             stat = "count", position = "stack",
                             ...,
                             width = NULL,
                             binwidth = NULL,
                             na.rm = FALSE,
                             orientation = NA,
                             show.legend = NA,
                             inherit.aes = TRUE) {

  if (!is.null(binwidth)) {
    warn("`geom_bar_pattern()` no longer has a `binwidth` parameter. Please use `geom_histogram()` instead.")
    return(geom_histogram(mapping = mapping, data = data,
                          position = position, width = width, binwidth = binwidth, ...,
                          na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBarPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomBarPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#' @include geom-rect.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomBarPattern <- ggproto(
  "GeomBarPattern", GeomRectPattern,
  required_aes = c("x", "y"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = FALSE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data <- transform(data,
                      ymin = pmin(y, 0), ymax = pmax(y, 0),
                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE) {
    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRectPattern, self)$draw_panel(data, panel_params, coord)
  }
)

