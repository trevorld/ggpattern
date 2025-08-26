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
      params = list(
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}


#' @export
#' @rdname ggpattern-ggproto
#' @usage NULL
#' @format NULL
GeomSfPattern <- ggproto("GeomSfPattern", GeomSf,
  required_aes = "geometry",
  default_aes = defaults(aes(shape = NULL, colour = NULL, fill = NULL, size = NULL, linewidth = NULL, linetype = 1,
      alpha = NA, stroke = 0.5),
    pattern_aesthetics
  ),

  draw_panel = function(self, data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        arrow = NULL, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      cli::cli_abort("{.fn {snake_class(self)}} can only be used with {.fn coord_sf}.")
    }

    # Need to refactor this to generate one grob per geometry type
    coord <- coord$transform(data, panel_params)
    sf_grob(coord, lineend = lineend, linejoin = linejoin, linemitre = linemitre,
            arrow = arrow, na.rm = na.rm, panel_params)
  },

  draw_key = function(data, params, size) {
    data <- modify_list(default_aesthetics(params$legend), data)
    if (params$legend == "point") {
      draw_key_point(data, params, size)
    } else if (params$legend == "line") {
      draw_key_path(data, params, size)
    } else {
      draw_key_polygon_pattern(data, params, size)
    }
  }
)

default_aesthetics <- function(type) {
  if (type == "point") {
    GeomPoint$default_aes
  } else if (type == "line") {
    GeomLine$default_aes
  } else  {
    modify_list(GeomPolygonPattern$default_aes, list(fill = "grey90", colour = "grey35"))
  }
}


# ggpattern note: panel params added to arguments
sf_grob <- function(x, lineend = "butt", linejoin = "round", linemitre = 10,
                   arrow = NULL, na.rm = TRUE, panel_params) {
  if (!requireNamespace("sf"))
      abort(c("Suggested package {sf} must be installed",
              i = 'Install using `install.packages("sf")`'))
  type <- sf_types[sf::st_geometry_type(x$geometry)]
  is_point <- type == "point"
  is_line <- type == "line"
  is_other <- type == "other"
  is_collection <- type == "collection"
  type_ind <- match(type, c("point", "line", "other", "collection"))
  remove <- rep_len(FALSE, nrow(x))
  remove[is_point] <- detect_missing(x, c(GeomPoint$required_aes, GeomPoint$non_missing_aes))[is_point]
  remove[is_line] <- detect_missing(x, c(GeomPath$required_aes, GeomPath$non_missing_aes))[is_line]
  remove[is_other] <- detect_missing(x, c(GeomPolygonPattern$required_aes, GeomPolygonPattern$non_missing_aes))[is_other]
  if (any(remove)) {
    if (!na.rm) {
      cli::cli_warn(paste0(
        "Removed {sum(remove)} row{?s} containing missing values or values ",
        "outside the scale range ({.fn geom_sf})."
      ))
    }
    x <- x[!remove, , drop = FALSE]
    type_ind <- type_ind[!remove]
    is_collection <- is_collection[!remove]
  }
  defaults <- list(
    GeomPoint$default_aes,
    GeomLine$default_aes,
    modify_list(GeomPolygonPattern$default_aes, list(fill = "grey90", colour = "grey35", linewidth = 0.2))
  )
  defaults[[4]] <- modify_list(
    defaults[[3]],
    rename(GeomPoint$default_aes, c(size = "point_size", fill = "point_fill"))
  )
  default_names <- unique0(unlist(lapply(defaults, names)))
  defaults <- lapply(setNames(default_names, default_names), function(n) {
    unlist(lapply(defaults, function(def) def[[n]] %||% NA))
  })
  alpha <- x$alpha %||% defaults$alpha[type_ind]
  col   <- x$colour %||% defaults$colour[type_ind]
  col[is_point | is_line] <- alpha(col[is_point | is_line], alpha[is_point | is_line])
  fill       <- x$fill %||% defaults$fill[type_ind]
  fill       <- fill_alpha(fill, alpha)
  size       <- x$size %||% defaults$size[type_ind]
  linewidth <- x$linewidth %||% defaults$linewidth[type_ind]
  point_size <- ifelse(
                       is_collection,
                       x$size %||% defaults$point_size[type_ind],
                       ifelse(is_point, size, linewidth)
  )
  stroke     <- (x$stroke %||% defaults$stroke[1]) * .stroke / 2
  fontsize   <- point_size * .pt + stroke
  lwd        <- ifelse(is_point, stroke, linewidth * .pt)
  pch        <- x$shape %||% defaults$shape[type_ind]
  lty        <- x$linetype %||% defaults$linetype[type_ind]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For each row in 'x',
  #  - if x$geometry is a MULTIPOLYGON then
  #       - treat x$geometry as the single item in the 'boundary_dfs' list
  #       - treat everything else in this row as params
  #       - create the pattern grobs for this isolated thing
  # - accumulate all these pattern grobs into a grobTree
  # - attach this grobTree to the final returned object
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sf_vp <- sf::st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), byrow=TRUE, ncol=2)))
  pattern_grobs_list <- list()
  gp_boundary <- gpar(col = NA, lwd = 0, fill = "white")
  for (idx in seq(nrow(x))) {
    if (inherits(x$geometry[[idx]], 'MULTIPOLYGON') || inherits(x$geometry[[idx]], 'POLYGON')) {
      boundary_grob      <- sf::st_as_grob(x$geometry[[idx]],
                                           gp = gp_boundary,
                                           default.units = "npc")
      if (inherits(boundary_grob, "null"))
          next
      boundary_grobs     <- list(boundary_grob)
      all_params         <- x[idx,]
      pattern_grobs      <- create_pattern_grobs(all_params, boundary_grobs)
      pattern_grobs_list <- append(pattern_grobs_list, list(pattern_grobs))
    }
  }
  pattern_grobs <- do.call(grid::grobTree, pattern_grobs_list)

  gp_fill <- gpar(
    col = NA, fill = fill, fontsize = fontsize, lwd = 0, lty = lty,
    lineend = lineend, linejoin = linejoin, linemitre = linemitre
  )
  gp_border <- gpar(
    col = col, fill = NA, fontsize = fontsize, lwd = lwd, lty = lty,
    lineend = lineend, linejoin = linejoin, linemitre = linemitre
  )
  grob_fill <- sf::st_as_grob(x$geometry, pch = pch, gp = gp_fill)
  grob_border <- sf::st_as_grob(x$geometry, pch = pch, gp = gp_border)
  grid::grobTree(
    grob_fill,
    pattern_grobs,
    grob_border
  )
}

sf_types <- c(GEOMETRY = "other", POINT = "point", LINESTRING = "line",
              POLYGON = "other", MULTIPOINT = "point", MULTILINESTRING = "line",
              MULTIPOLYGON = "other", GEOMETRYCOLLECTION = "collection",
              CIRCULARSTRING = "line", COMPOUNDCURVE = "line", CURVEPOLYGON = "other",
              MULTICURVE = "line", MULTISURFACE = "other", CURVE = "line",
              SURFACE = "other", POLYHEDRALSURFACE = "other", TIN = "other",
              TRIANGLE = "other")
