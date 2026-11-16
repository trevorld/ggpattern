#' Geom ggproto objects
#'
#' Geom ggproto objects that could be extended to create a new geom.
#'
#' @details
#' Each of these extends a `{ggplot2}` geom, either directly (e.g. `GeomRectPattern` extends [ggplot2::GeomRect])
#' or through another of these (e.g. `GeomColPattern` extends `GeomBarPattern` which extends `GeomRectPattern`),
#' and uses the methods (e.g. the draw method) of the `{ggplot2}` that is loaded.
#'
#' However, a few things are copied from `{ggplot2}` when `{ggpattern}` is installed:
#'
#' * The default aesthetics of `GeomDotplotPattern`, `GeomHexPattern`, `GeomPointPattern`, and `GeomSfPattern`
#'   are those of their `{ggplot2}` geom (plus the pattern aesthetics).
#' * The arguments of the `geom_*_pattern()` functions made by [ggplot2::make_constructor()]
#'   (e.g. [geom_hex_pattern()]) are those of the geom and stat's parameters.
#'
#' So after upgrading `{ggplot2}`, reinstall `{ggpattern}` to pick up any new default aesthetics or arguments.
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

  setup_params = function(self, data, params) ggproto_parent(GeomBar, self)$setup_params(data, params),

  extra_params = c("just", "na.rm", "orientation"),

  setup_data = function(self, data, params) ggproto_parent(GeomBar, self)$setup_data(data, params),

  rename_size = FALSE
)


#' @rdname geom-docs
#' @export
geom_bar_pattern <- make_constructor(
  GeomBarPattern,
  stat = "count", position = "stack", just = 0.5
)
