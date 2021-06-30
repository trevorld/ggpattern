
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' ggplot2 geoms with support for pattern fills
#'
#' All geoms in this package are identical to their counterparts in ggplot2 except
#' that they can be filled with patterns.
#'
#' @usage NULL
#'
#' @section Pattern Arguments:
#'
#' Not all arguments apply to all patterns.
#'
#' \describe{
#' \item{\strong{\code{pattern}}}{Pattern name string e.g. 'stripe' (default), 'crosshatch', 'point', 'circle', 'none'}
#' \item{\strong{\code{pattern_colour       }}}{ Colour used for strokes and points. default: 'black'}
#' \item{\strong{\code{pattern_fill         }}}{ Fill colour. default: 'black'}
#' \item{\strong{\code{pattern_angle        }}}{ Orientation of the pattern in degrees. default: 45}
#' \item{\strong{\code{pattern_density      }}}{ Approximate fill fraction of the pattern. Usually in range \[0, 1], but can be higher. default: 0.2}
#' \item{\strong{\code{pattern_spacing      }}}{ Spacing of the pattern as a fraction of the plot size. default: 0.05}
#' \item{\strong{\code{pattern_xoffset,pattern_yoffset}}}{Offset the origin of the pattern. Range \[0, 1]. default: 0.  Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#' \item{\strong{\code{pattern_alpha        }}}{ Alpha transparency for pattern. default: 1}
#' \item{\strong{\code{pattern_linetype     }}}{ Stroke linetype. default: 1}
#' \item{\strong{\code{pattern_size         }}}{ Stroke line width. default: 1}
#' \item{\strong{\code{pattern_option_1     }}}{ Generic User value }
#' }
#'
#' @param mapping,data,stat,position,linejoin,na.rm,show.legend,inherit.aes,binwidth
#' See ggplot2 for description of the standard arguments to each geom.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_rect_pattern <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              linejoin = "mitre",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRectPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomRectPattern
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomRectPattern <- ggplot2::ggproto(
  "GeomRectPattern", GeomRect,

  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      colour          = NA,
      fill            = "grey35",
      size            = 0.5,
      linetype        = 1,
      alpha           = NA
    )
  ),

  aspect_ratio = 1,

  draw_key = function(self, ...) {
    draw_key_polygon_pattern(..., aspect_ratio = self$aspect_ratio)
  },

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    if (!coord$is_linear()) {
      aesthetics <- setdiff(
        names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax")
      )

      polys <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- new_data_frame(row[aesthetics])[rep(1,5), ]

        GeomPolygonPattern$draw_panel(cbind(poly, aes), panel_params, coord)
      })

      ggname("bar", do.call("grobTree", polys))
    } else {
      coords <- coord$transform(data, panel_params)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Calculate all the boundary_dfs for all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      boundary_dfs <- lapply(seq(nrow(coords)), function(i) {
        params <- coords[i,]
        create_polygon_df(
          y = with(params, c(ymax, ymax, ymin, ymin, ymax)),
          x = with(params, c(xmin, xmax, xmax, xmin, xmin))
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # For rectangles, every row in coords represents an element.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      all_params <- coords

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create the pattern grobs given the current params for every element
      # (given in coords), and the boundary_dfs of all the elements
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      self$aspect_ratio <- get_aspect_ratio()
      pattern_grobs <- create_pattern_grobs(all_params, boundary_dfs, self$aspect_ratio)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adapt the returned geom to always be a grobTree with the
      # pattern_grobs as the final element. Since the pattern grobs are
      # drawn last, there can be z-ordering issues that the user will have
      # to handle manually if there are overlapping rects
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ggname(
        "geom_rect",
        grid::grobTree(
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # The area fill of the rect
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          grid::rectGrob(
            coords$xmin, coords$ymax,
            width         = coords$xmax - coords$xmin,
            height        = coords$ymax - coords$ymin,
            default.units = "native",
            just          = c("left", "top"),
            gp = grid::gpar(
              col      = NA,
              fill     = alpha(coords$fill, coords$alpha),
              lwd      = coords$size * .pt,
              lty      = coords$linetype,
              linejoin = linejoin,
              lineend  = if (identical(linejoin, "round")) "round" else "square"
            )
          ),

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # The pattern over the top of the fill
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          pattern_grobs,

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # The edge of the rect
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          grid::rectGrob(
            coords$xmin, coords$ymax,
            width         = coords$xmax - coords$xmin,
            height        = coords$ymax - coords$ymin,
            default.units = "native",
            just          = c("left", "top"),
            gp = grid::gpar(
              col      = coords$colour,
              fill     = NA,
              lwd      = coords$size * .pt,
              lty      = coords$linetype,
              linejoin = linejoin,
              lineend  = if (identical(linejoin, "round")) "round" else "square"
            )
          )
        )
      )
    }
  }
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert rectangle to polygon
# Useful for non-Cartesian coordinate systems where it's easy to work purely in
# terms of locations, rather than locations and dimensions. Note that, though
# `polygonGrob()` expects an open form, closed form is needed for correct
# munching (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-458406857).
#
# @keyword internal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
}



if (FALSE) {
  library(ggplot2)
  library(dplyr)

  plot_df <- data.frame(
    xmin    = c(0, 10),
    xmax    = c(8, 18),
    ymin    = c(0, 10),
    ymax    = c(5, 19),
    type    = c('a', 'b'),
    angle   = c(45, 0),
    pname   = c('circle', 'circle'),
    pcolour = c('red', 'blue'),
    pspace  = c(0.03, 0.05),
    stringsAsFactors = FALSE
  )


  p <- ggplot(plot_df) +
    geom_rect_pattern(
      aes(
        xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
        # pattern         = I(pname),
        pattern_angle   = I(angle),
        pattern_colour  = I(pcolour),
        pattern_spacing = I(pspace)
      ),
      pattern         = 'circle',
      fill            = 'white',
      colour          = 'black',
      pattern_density = 0.3
    ) +
    theme_bw() +
    labs(title = "ggpattern::geom_rect_pattern()")




  pdf("working/test.pdf")
  p
  dev.off()


}
