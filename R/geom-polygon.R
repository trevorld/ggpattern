#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_polygon_pattern <- function(mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 rule = "evenodd",
                                 ...,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPolygonPattern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      rule  = rule,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomPolygonPattern <- ggproto("GeomPolygonPattern", GeomPolygon,

  draw_panel = function(self, data, panel_params, coord, rule = "evenodd") {
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- ggplot2::coord_munch(coord, data, panel_params)

    if (is.null(munched$subgroup)) {
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Calculate all the boundary_dfs for all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stopifnot(!is.null(munched$group))
      polygons <- split(munched, munched$group)
      boundary_dfs <- lapply(polygons, function(polygon) {
        create_polygon_df(
          x = polygon$x,
          y = polygon$y
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # For polygons, every row in first_rows represents an element.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      all_params <- first_rows

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create the pattern grobs given the current params for every element
      # (given in all_params), and the boundary_dfs of all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$aspect_ratio <- get_aspect_ratio()
      pattern_grobs <- create_pattern_grobs(all_params, boundary_dfs, self$aspect_ratio)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adapt the returned geom to always be a grobTree with the
      # pattern_grobs as the final element. Since the pattern grobs are
      # drawn last, there can be z-ordering issues that the user will have
      # to handle manually if there are overlapping elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      col <- first_rows$colour
      fill <- scales::alpha(first_rows$fill, first_rows$alpha)
      lwd <- first_rows$linewidth * .pt

      polygon_grob_fn <- function(col, fill, lwd) {
          grid::polygonGrob(
            munched$x, munched$y, default.units = "native",
            id = munched$group,
            gp = grid::gpar(col = col, fill = fill, lwd = lwd,
                            lty  = first_rows$linetype)
          )
      }
      ggname(
        "geom_polygon",
        grid::grobTree(
          polygon_grob_fn(NA, fill, 0),
          # verboseGrob("polygon"),
          pattern_grobs,
          polygon_grob_fn(col, NA, lwd)
        )
      )
    } else {
      if (utils::packageVersion('grid') < "3.6") {
        abort("Polygons with holes requires R 3.6 or above")
      }

      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique(munched$subgroup))

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Calculate all the boundary_dfs for all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      stopifnot(!is.null(munched$group))
      polygons <- split(munched, munched$group)
      boundary_grobs <- lapply(polygons, function(polygon) {
          grid::pathGrob(polygon$x, polygon$y,
                         default.units = "npc",
                         gp = gpar(col = NA, lwd = 0, fill = "white"),
                         id = polygon$subgroup, rule = rule)
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # For polygons, every row in first_rows represents an element.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      all_params <- first_rows

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create the pattern grobs given the current params for every element
      # (given in all_params), and the boundary_dfs of all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$aspect_ratio <- get_aspect_ratio()
      pattern_grobs <- create_pattern_grobs(all_params,
                                            boundary_grobs,
                                            self$aspect_ratio)

      gp_fill <- grid::gpar(
         col  = NA,
         fill = scales::alpha(first_rows$fill, first_rows$alpha),
         lwd  = first_rows$linewidth * .pt,
         lty  = first_rows$linetype
       )
      gp_border <- grid::gpar(
          col  = first_rows$colour,
          fill = NA,
          lwd  = first_rows$linewidth * .pt,
          lty  = first_rows$linetype
        )
      path_grob_fn <- function(gp = gpar()) {
          grid::pathGrob(
            munched$x, munched$y, default.units = "native",
            id = id, pathId = munched$group,
            rule = rule,
            gp = gp)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adapt the returned geom to always be a grobTree with the
      # pattern_grobs as the final element. Since the pattern grobs are
      # drawn last, there can be z-ordering issues that the user will have
      # to handle manually if there are overlapping elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ggname(
        "geom_polygon",
        grid::grobTree(
          path_grob_fn(gp_fill), # area filled of the polygon
          pattern_grobs, # the pattern fill
          path_grob_fn(gp_border) # the edge of the polygon
        )
      )
    }

  },

  aspect_ratio = 1,

  draw_key = function(self, ...) {
    draw_key_polygon_pattern(..., aspect_ratio = self$aspect_ratio)
  },

  default_aes = augment_aes(
    pattern_aesthetics,
    ggplot2::aes(
      colour           = "NA",
      fill             = "grey20",
      linewidth        = 0.5,
      linetype         = 1,
      alpha            = NA,
      subgroup         = NULL,
    )
  ),

  rename_size = TRUE
)

if (FALSE) {
  library(ggplot2)
  ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

  values <- data.frame(
    id = ids,
    value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
  )

  positions <- data.frame(
    id = rep(ids, each = 4),
    x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
          0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
    y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
          2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
  )

  # Currently we need to manually merge the two together
  datapoly <- merge(values, positions, by = c("id"))

  p <- ggplot(datapoly, aes(x = x, y = y)) +
    geom_polygon_pattern(aes(fill = value, group = id), col = "red", lwd = 2)
  p

}














