

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname create_pattern_none
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_crosshatch_via_sf <- function(params, boundary_df, aspect_ratio,
                                             legend = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create stripes in 1 direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripes_sf <- create_stripes_sf(
    angle     = params$pattern_angle,
    spacing   = params$pattern_spacing,
    density   = params$pattern_density,
    xoffset   = params$pattern_xoffset,
    yoffset   = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)

  striped_area     <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons1 <- convert_polygon_sf_to_polygon_df(striped_area)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create stripes in other direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripes_sf <- create_stripes_sf(
    angle    = params$pattern_angle + 90,
    spacing  = params$pattern_spacing,
    density  = params$pattern_density,
    xoffset  = params$pattern_xoffset,
    yoffset  = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )
  striped_area     <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons2 <- convert_polygon_sf_to_polygon_df(striped_area)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Join all stripe polygons, and make sure the IDs don't overlap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(stripe_polygons1) && is.null(stripe_polygons2)) {
    return(grid::nullGrob())
  } else if (is.null(stripe_polygons1)) {
    crosshatch_polygons  <- stripe_polygons2
  } else if (is.null(stripe_polygons2)) {
    crosshatch_polygons  <- stripe_polygons1
  } else {
    stripe_polygons2$id <- stripe_polygons2$id + max(stripe_polygons1$id)
    crosshatch_polygons  <- rbind(stripe_polygons1, stripe_polygons2)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  crosshatch_grob <- grid::polygonGrob(
    x = unit(crosshatch_polygons$x, "npc"),
    y = unit(crosshatch_polygons$y, "npc"),
    id = crosshatch_polygons$id,
    gp = gpar(
      col     = scales::alpha(params$pattern_colour, params$pattern_alpha),
      fill    = scales::alpha(params$pattern_fill  , params$pattern_alpha),
      lwd     = params$pattern_size * .pt,
      lty     = params$pattern_linetype,
      lineend = 'square'
    )
  )

  crosshatch_grob
}
