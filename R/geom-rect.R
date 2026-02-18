#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRectPattern <- ggproto( "GeomRectPattern", GeomRect,
  default_aes = defaults(aes(
	colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper, 0.35)),
    linewidth = from_theme(borderwidth), linetype = from_theme(bordertype),
    alpha = NA
  ),
    pattern_aesthetics
  ),

  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    data <- fix_linewidth(data, snake_class(self))
    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )
      index <- rep(seq_len(nrow(data)), each = 4)

      new <- data[index, aesthetics, drop = FALSE]
      new$x <- vec_interleave(data$xmin, data$xmax, data$xmax, data$xmin)
      new$y <- vec_interleave(data$ymax, data$ymax, data$ymin, data$ymin)
      new$group <- index

      ggname("geom_rect_pattern", GeomPolygonPattern$draw_panel(
        new, panel_params, coord, lineend = lineend, linejoin = linejoin
      ))
    } else {
      coords <- coord$transform(data, panel_params)

      boundary_dfs <- lapply(seq(nrow(coords)), function(i) {
        params <- coords[i,]
        create_polygon_df(
          y = with(params, c(ymax, ymax, ymin, ymin, ymax)),
          x = with(params, c(xmin, xmax, xmax, xmin, xmin))
        )
      })
      pattern_grobs <- create_pattern_grobs(coords, boundary_dfs)

      rect_grob_fn <- function(col, fill, lwd) {
          rectGrob(
            coords$xmin, coords$ymax,
            width         = coords$xmax - coords$xmin,
            height        = coords$ymax - coords$ymin,
            default.units = "native",
            just          = c("left", "top"),
            gp = gpar(
              col = col,
              fill = fill,
              lwd = lwd,
              lty = coords$linetype,
              linejoin = linejoin,
              lineend = lineend
            )
          )
      }

      ggname("geom_rect_pattern", grobTree(
          rect_grob_fn(NA, fill_alpha(coords$fill, coords$alpha), 0),
          pattern_grobs,
          rect_grob_fn(coords$colour, NA, coords$linewidth * .pt)
        )
      )
    }
  },

  draw_key = draw_key_polygon_pattern,
)

#' @export
#' @rdname geom-docs
geom_rect_pattern <- make_constructor(GeomRectPattern)
