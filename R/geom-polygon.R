#' @rdname geom-docs
#' @export
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
    params = list2(
      na.rm = na.rm,
      rule  = rule,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPolygonPattern <- ggproto("GeomPolygonPattern", GeomPolygon,
  draw_panel = function(self, data, panel_params, coord, rule = "evenodd",
                        lineend = "butt", linejoin = "round", linemitre = 10) {
    data <- check_linewidth(data, snake_class(self))
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params, is_closed = TRUE)

    if (is.null(munched$subgroup)) {
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group), ]

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      stopifnot(!is.null(munched$group))
      polygons <- split(munched, munched$group)
      boundary_dfs <- lapply(polygons, function(polygon) {
        create_polygon_df(
          x = polygon$x,
          y = polygon$y
        )
      })
      pattern_grobs <- create_pattern_grobs(first_rows, boundary_dfs)

      col <- first_rows$colour
      fill <- fill_alpha(first_rows$fill, first_rows$alpha)
      lwd <- first_rows$linewidth * .pt

      polygon_grob_fn <- function(col, fill, lwd) {
          polygonGrob(
            munched$x, munched$y, default.units = "native",
            id = munched$group,
            gp = gpar(
              col = col,
              fill = fill,
              lwd = lwd,
              lty  = first_rows$linetype,
              lineend = lineend,
              linejoin = linejoin,
              linemitre = linemitre
            )

          )
      }
      ggname(
        "geom_polygon",
        grobTree(
          polygon_grob_fn(NA, fill, 0),
          pattern_grobs,
          polygon_grob_fn(col, NA, lwd)
        )
      )
    } else {
      if (getRversion() < "3.6") {
        cli::cli_abort("Polygons with holes requires R 3.6 or above.")
      }
      # Sort by group to make sure that colors, fill, etc. come in same order
      munched <- munched[order(munched$group, munched$subgroup), ]
      id <- match(munched$subgroup, unique0(munched$subgroup))

      # For gpar(), there is one entry per polygon (not one entry per point).
      # We'll pull the first value from each group, and assume all these values
      # are the same within each group.
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      stopifnot(!is.null(munched$group))
      polygons <- split(munched, munched$group)
      boundary_grobs <- lapply(polygons, function(polygon) {
          grid::pathGrob(polygon$x, polygon$y,
                         default.units = "npc",
                         gp = gpar(col = NA, lwd = 0, fill = "white"),
                         id = polygon$subgroup, rule = rule)
      })

      pattern_grobs <- create_pattern_grobs(first_rows, boundary_grobs)

      gp_fill <- grid::gpar(
         col  = NA,
         fill = fill_alpha(first_rows$fill, first_rows$alpha),
         lwd  = 0
       )
      gp_border <- grid::gpar(
          col  = first_rows$colour,
          fill = NA,
          lwd  = first_rows$linewidth * .pt,
          lty  = first_rows$linetype,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      path_grob_fn <- function(gp = gpar()) {
          grid::pathGrob(
            munched$x, munched$y, default.units = "native",
            id = id, pathId = munched$group,
            rule = rule,
            gp = gp)
      }

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

  default_aes = defaults(aes(colour = NA, fill = "grey20", linewidth = 0.5, linetype = 1,
                             alpha = NA, subgroup = NULL),
    pattern_aesthetics
  ),


  draw_key = function(self, ...) draw_key_polygon_pattern(...),

  rename_size = TRUE
)
