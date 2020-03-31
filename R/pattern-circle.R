


#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a cicleGrob object for a set of points
#'
#' Use 'sf' to help with the point in polygon intersections.
#'
#' \itemize{
#'   \item{make grid to cover entire space}
#'   \item{rotate points into position}
#'   \item{create expanded boundary by r}
#'   \item{create contracted boundary by r}
#'   \item{remove all points outside the expanded boundary}
#'   \item{remove all points within contracted boundary -> internal circles}
#'   \item{any remaining points become part of the intersection grob}
#'   \item{total circles = treeGrob( internal_circls, intersection_circles)}
#' }
#'
#' @param boundary_df polygon_df data.frame
#' @param angle angle of orientation (degrees)
#' @param spacing spacing in grid 'npc' coordinates. Usually in range [0, 1]
#' @param density fill fraction. Number in range [0, 1]
#' @param xoffset,yoffset offset the pattern creation origin.
#' @param aspect_ratio aspect_ratio
#' @param params params from the geom
#'
#' @return A grid::circleGrob
#'
#' @import grid
#' @import sf
#' @import grid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_circles_grob <- function(boundary_df, params, angle=0, spacing=0.1, density=0.3,
                                xoffset=0, yoffset=0,
                                aspect_ratio) {

  angle <- angle %% 90

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate radius
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r <- spacing * density / 2
  if (aspect_ratio > 1) {
    r <- r * aspect_ratio
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Things get dicey at the boundaries, especially when there is very large
  # or small aspect ratio.   Include this fudge factor in buffering the
  # boundary to ensure that all partially ntersecting circles are kept
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fudge_factor <- aspect_ratio
  if (fudge_factor < 1) {
    fudge_factor <- 1/fudge_factor
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generate a square grid of points
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rff <- r * fudge_factor
  yff <- 1 / aspect_ratio * 1

  yff <- max(yff, 2)


  point_coords <- expand.grid(
    x = seq(-rff    , yff+rff, spacing),
    y = seq(-yff-rff, yff+rff, spacing)
  )

  if (nrow(point_coords) == 0) {
    return(grid::nullGrob())
  }

  point_coords   <- rotate_polygon_df(point_coords, angle, aspect_ratio)
  point_coords$y <- point_coords$y * aspect_ratio

  points_sf    <- sf::st_multipoint(as.matrix(point_coords))

  boundary_sf   <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist =  0)
  expanded_sf   <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist =  r * fudge_factor)
  contracted_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = -r * fudge_factor)

  all_points_sf      <- sf::st_intersection(expanded_sf  , points_sf)
  interior_points_sf <- sf::st_intersection(contracted_sf, all_points_sf)
  exterior_points_sf <- sf::st_difference(all_points_sf, contracted_sf)

  interior_points_mat <- as.matrix(interior_points_sf)
  exterior_points_mat <- as.matrix(exterior_points_sf)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grob for the internal circles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(interior_points_mat) || nrow(interior_points_mat) == 0) {
    interior_circles_grob <- nullGrob()
  } else {
    interior_circles_grob <- grid::circleGrob(
      x = interior_points_mat[,1],
      y = interior_points_mat[,2],
      r = r,
      gp = gpar(
        fill = scales::alpha(params$pattern_fill  , params$pattern_alpha),
        col  = scales::alpha(params$pattern_colour, params$pattern_alpha),
        lwd  = params$pattern_size * .pt,
        lty  = params$pattern_linetype
      )
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grob for the intersecting circles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(exterior_points_mat) || nrow(exterior_points_mat) == 0) {
    exterior_circles_grob <- nullGrob()
  } else {
    exterior_circles_grob <- grid::circleGrob(
      x = exterior_points_mat[,1],
      y = exterior_points_mat[,2],
      r = r
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Find the intersection of the boundary and the minimal set of points I
    # have called the 'exterior points'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    boundary_grob <- convert_polygon_df_to_polygon_grob(boundary_df)
    exterior_circles_grob <- gridGeometry::polyclipGrob(
      boundary_grob, exterior_circles_grob,
      gp = gpar(
        fill = scales::alpha(params$pattern_fill  , params$pattern_alpha),
        col  = scales::alpha(params$pattern_colour, params$pattern_alpha),
        lwd  = params$pattern_size * .pt,
        lty  = params$pattern_linetype
      )
    )
  }


  grid::grobTree(
    interior_circles_grob,
    exterior_circles_grob
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname create_pattern_none
#' @importFrom gridGeometry polyclipGrob
#' @import scales
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_circles <- function(params, boundary_df, aspect_ratio, legend = FALSE) {

  stopifnot(is_polygon_df(boundary_df))

  boundary_grob <- convert_polygon_df_to_polygon_grob(boundary_df)
  bbox          <- calculate_bbox_polygon_df(boundary_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an SF object with points covering the entire viewpoint
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  circle_grob <- create_circles_grob(
    boundary_df  = boundary_df,
    params       = params,
    angle        = params$pattern_angle,
    spacing      = params$pattern_spacing,
    density      = params$pattern_density,
    xoffset      = params$pattern_xoffset,
    yoffset      = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  circle_grob
}




if (FALSE) {
  df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
  df <- data.frame(trt = c("a", 'b'), outcome = c(2.3, 1.9))
  df <- data.frame(trt = c("a"), outcome = c(2.3))

  ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(
      aes(fill=trt),
      colour          = 'black',
      pattern         = 'circle',
      pattern_fill    = 'green',
      pattern_angle   = 30,
      pattern_density = 1,
      pattern_spacing = 0.08
    ) +
    theme_bw() +
    labs(title = "ggpattern::geom_col_pattern()") +
    theme(legend.position = 'none') +
    coord_fixed(ratio = 2)
}






if (FALSE) {
  library(grid)
  library(ggplot2)

  circle_grob <- create_circles_grob(density = 0.6, aspect_ratio = 1)
  plot.new()
  grid.draw(circle_grob)


  d <- 0.1
  boundary_df <- create_polygon_df(x = c(d, 1-d, 1-d, d), y = c(d, d, 1-d, 1-d))

  boundary_grob <- convert_polygon_df_to_polygon_grob(boundary_df)

  int_grob <- gridGeometry::polyclipGrob(boundary_grob, circle_grob, gp = gpar(fill = 'blue', colour = 'blue', size = 2))

  plot.new()
  grid.draw(boundary_grob)
  grid.draw(circle_grob)
  grid.draw(int_grob)
}















