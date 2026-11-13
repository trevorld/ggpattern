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
    grob <- ggproto_parent(GeomSf, self)$draw_panel(
      data, panel_params, coord, legend = legend, lineend = lineend,
      linejoin = linejoin, linemitre = linemitre, arrow = arrow,
      arrow.fill = arrow.fill, na.rm = na.rm
    )
    # (a `gList` so that `points` (e.g. of a `MULTIPOINT` layer) aren't patterned)
    if (is.grob(grob)) grob <- gList(grob)

    # One pattern per row filling all its polygons as a single region
    # (except the polygons of a geometry collection which are each their own grob)
    rows <- sf_area_rows(grob, data$geometry)
    if (length(rows) > 0L) {
      pool <- sf_area_pools(rows, data$geometry)
      grob <- with_pattern_aes(grob, data[rows[!duplicated(pool)], , drop = FALSE], pool = pool)
    }
    ggname("geom_sf_pattern", gTree(children = grob))
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

# The row of each area (in the order of `gridpattern::area_boundaries()`)
# of the `gList` `grobs` that `sf::st_as_grob()` drew for `geometry`
sf_area_rows <- function(grobs, geometry) {
  if (length(grobs) == length(geometry)) {
    # (a grob per row)
    n_row <- vapply(grobs, function(g) length(gridpattern::area_boundaries(gList(g))), integer(1L))
  } else {
    # a single `pathgrob` of every non-empty (multi)polygon with a path per polygon
    if (length(grobs) != 1L || !inherits(grobs[[1L]], "pathgrob")) abort_sf_areas(grobs, geometry)
    path <- grobs[[1L]]
    n_row <- ifelse(sf::st_is_empty(geometry), 0L,
                    ifelse(sf::st_is(geometry, "MULTIPOLYGON"), lengths(geometry), 1L))
    n_paths <- length(path$pathId.lengths %||% unique(path$pathId %||% 1L))
    if (sum(n_row) != n_paths) abort_sf_areas(grobs, geometry)
  }
  rep(seq_along(geometry), n_row)
}

# (`sf::st_as_grob()` drew `geometry` in a way we don't know how to match to its rows)
abort_sf_areas <- function(grobs, geometry) {
  cli::cli_abort(c(
    "Can't match the areas drawn by {.fn sf::st_as_grob} to the rows of the data.",
    i = "It drew {length(grobs)} grob{?s} for {length(geometry)} geometr{?y/ies} of class {.cls {class(geometry)[1L]}}.",
    i = "Please report this at {.url https://github.com/trevorld/ggpattern/issues}."
  ))
}

# The `pool` of each area (areas with the same `pool` are filled as one region by the same pattern):
# its row except areas of a geometry collection (each their own grob)
sf_area_pools <- function(rows, geometry) {
  is_collection <- sf::st_is(geometry, "GEOMETRYCOLLECTION")[rows]
  pool <- as.character(rows)
  pool[is_collection] <- paste(rows[is_collection], seq_along(rows)[is_collection], sep = ".")
  vec_group_id(pool)
}
