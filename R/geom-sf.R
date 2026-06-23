#' @export
#' @rdname ggpattern-ggproto
#' @usage NULL
#' @format NULL
GeomSfPattern <- ggproto("GeomSfPattern", GeomSf,
  required_aes = "geometry",
  default_aes = defaults(aes(
    shape = NULL,
    colour = NULL,
    fill = NULL,
    size = NULL,
    linewidth = NULL,
    linetype = NULL,
    alpha = NA,
    stroke = 0.5
  ),
    pattern_aesthetics
  ),

  draw_panel = function(self, data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        arrow = NULL, arrow.fill = NULL, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      cli::cli_abort("{.fn {snake_class(self)}} can only be used with {.fn coord_sf}.")
    }
    data$shape <- translate_shape_string(data$shape)

    data <- coord$transform(data, panel_params)

    type <- sf_types[sf::st_geometry_type(data$geometry)]
    is_point <- type == "point"
    is_line  <- type == "line"
    is_collection <- type == "collection"

    fill <- fill_alpha(data$fill %||% rep(NA, nrow(data)), data$alpha)
    fill[is_line] <- arrow.fill %||% fill[is_line]

    colour <- data$colour
    colour[is_point | is_line] <-
      alpha(colour[is_point | is_line], data$alpha[is_point | is_line])

    point_size <- data$size
    point_size[!(is_point | is_collection)] <-
      data$linewidth[!(is_point | is_collection)]

    stroke <- (data$stroke %||% rep(0.5, nrow(data))) * .stroke / 2
    font_size <- point_size * .pt + stroke

    linewidth <- data$linewidth * .pt
    linewidth[is_point] <- stroke[is_point]

    # Render pattern grobs for polygon geometries
    pattern_grobs_list <- list()
    gp_boundary <- gpar(col = NA, lwd = 0, fill = "white")
    for (idx in seq_len(nrow(data))) {
      geom_obj <- data$geometry[[idx]]
      if (inherits(geom_obj, 'MULTIPOLYGON') || inherits(geom_obj, 'POLYGON')) {
        boundary_grob <- sf::st_as_grob(geom_obj, gp = gp_boundary, default.units = "npc")
        if (inherits(boundary_grob, "null")) next
        pattern_grobs_list <- append(
          pattern_grobs_list,
          list(create_pattern_grobs(data[idx, ], list(boundary_grob)))
        )
      }
    }
    pattern_grobs <- do.call(grid::grobTree, pattern_grobs_list)

    gp_fill <- gpar(
      col = NA, fill = fill, fontsize = font_size, lwd = 0, lty = data$linetype,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    gp_border <- gpar(
      col = colour, fill = NA, fontsize = font_size, lwd = linewidth, lty = data$linetype,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
    grob_fill   <- sf::st_as_grob(data$geometry, pch = data$shape, gp = gp_fill)
    grob_border <- sf::st_as_grob(data$geometry, pch = data$shape, gp = gp_border, arrow = arrow)

    grid::grobTree(grob_fill, pattern_grobs, grob_border)
  },

  draw_key = function(data, params, size) {
    switch(
      params$legend %||% "other",
      point = draw_key_point(data, params, size),
      line  = draw_key_path(data, params, size),
      draw_key_polygon_pattern(data, params, size)
    )
  }
)

#' @rdname geom-docs
#' @export
geom_sf_pattern <- function(mapping = aes(), data = NULL, stat = "sf",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  c(
    layer_sf(
      geom = GeomSfPattern,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list2(
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}


sf_types <- c(GEOMETRY = "other", POINT = "point", LINESTRING = "line",
              POLYGON = "other", MULTIPOINT = "point", MULTILINESTRING = "line",
              MULTIPOLYGON = "other", GEOMETRYCOLLECTION = "collection",
              CIRCULARSTRING = "line", COMPOUNDCURVE = "line", CURVEPOLYGON = "other",
              MULTICURVE = "line", MULTISURFACE = "other", CURVE = "line",
              SURFACE = "other", POLYHEDRALSURFACE = "other", TIN = "other",
              TRIANGLE = "other")
