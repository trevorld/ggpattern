#' @include geom-polygon.R
NULL

#' @rdname geom-docs
#' @export
geom_map_pattern <- function(mapping = NULL, data = NULL,
                             stat = "identity",
                             ...,
                             map,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Get map input into correct form
  stopifnot(is.data.frame(map))
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  if (!all(c("x", "y", "id") %in% names(map))) {
    cli::cli_abort("{.arg map} must have the columns {.col x}, {.col y}, and {.col id}.")
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMapPattern,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomMapPattern <- ggproto("GeomMapPattern", GeomPolygonPattern,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        linejoin = "round", linemitre = 10, map) {
    grob <- GeomMap$draw_panel(data, panel_params, coord, lineend = lineend,
                               linejoin = linejoin, linemitre = linemitre, map = map)

    # Align data with map exactly as `GeomMap$draw_panel()` does:
    # one polygon per map group in order of first appearance in the *munched* coords
    # (in non-linear coords `coord_munch()` sorts closed polygons by group)
    common <- intersect(data$map_id, map$id)
    data   <- data[data$map_id %in% common, , drop = FALSE]
    map    <- map[map$id %in% common, , drop = FALSE]
    coords <- coord_munch(coord, map, panel_params, is_closed = TRUE)
    coords$group <- coords$group %||% coords$id
    grob_id <- match(coords$group, unique0(coords$group))
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)

    ggname("geom_map_pattern", with_pattern_rows(grob, data[data_rows, , drop = FALSE]))
  },

  required_aes = c("map_id")
)
