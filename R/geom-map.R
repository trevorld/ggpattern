#' @include geom-polygon.R
NULL

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_map_pattern <- function(mapping = NULL, data = NULL,
                             stat = "identity",
                             ...,
                             map,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  # Get map input into correct form
  if (!is.data.frame(map)) {
    abort("`map` must be a data.frame")
  }
  if (!is.null(map$lat)) map$y <- map$lat
  if (!is.null(map$long)) map$x <- map$long
  if (!is.null(map$region)) map$id <- map$region
  if (!all(c("x", "y", "id") %in% names(map))) {
    abort("`map` must have the columns `x`, `y`, and `id`")
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMapPattern,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      map = map,
      na.rm = na.rm,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomMapPattern <- ggproto(
  "GeomMapPattern", GeomPolygonPattern,
  draw_panel = function(data, panel_params, coord, map) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Only use matching data and map ids
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    common <- intersect(data$map_id, map$id)
    data   <- data[data$map_id %in% common, , drop = FALSE]
    map    <- map[map$id %in% common, , drop = FALSE]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Munch, then set up id variable for polygonGrob -
    # must be sequential integers
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    coords       <- coord_munch(coord, map, panel_params)
    coords$group <- coords$group %||% coords$id
    grob_id      <- match(coords$group, unique(coords$group))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Align data with map
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_rows <- match(coords$id[!duplicated(grob_id)], data$map_id)
    data      <- data[data_rows, , drop = FALSE]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate all the boundary_dfs for all the elements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygons <- split(coords, coords$group)
    boundary_dfs <- lapply(polygons, function(polygon) {
      create_polygon_df(
        x = polygon$x,
        y = polygon$y
      )
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate pattern grobs
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    all_params    <- data
    pattern_grobs <- grid::nullGrob()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create the pattern grobs given the current params for every element
    # (given in all_params), and the boundary_dfs of all the elements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pattern_grobs <- create_pattern_grobs(all_params, boundary_dfs)

    col <- data$colour
    fill <- fill_alpha(data$fill, data$alpha)
    lwd <- data$linewidth * .pt

    base_grob_fn <- function(col, fill, lwd) {
      grid::polygonGrob(
        x = coords$x,
        y = coords$y,
        default.units = "native",
        id = grob_id,
        gp = gpar(col = col, fill = fill, lwd = lwd)
      )
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Tree - final assembled grob tree
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    grid::grobTree(
      base_grob_fn(NA, fill, NA),
      pattern_grobs,
      base_grob_fn(col, NA, lwd)
    )
  },

  required_aes = c("map_id")
)
