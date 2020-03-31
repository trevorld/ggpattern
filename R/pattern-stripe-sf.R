

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a sf MULTIPOLYGON object where each polygon is an individual stripe.
#'
#' The stripes are created as polygons so that when clipped to rects/polygons,
#' the ends of the stripe are clipped correctly to the boundary.
#'
#' @inheritParams create_circles_grob
#'
#' @return `sf` multipolygon object
#'
#' @import sf
#' @importFrom utils tail
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stripes_sf <- function(angle, spacing, density, xoffset=0, yoffset=0,
                                               aspect_ratio) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clearly distinguise:
  #    - The user supplies the stripe angle.
  #    - We determine the spine angle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_angle <- ( angle       %% 180)
  spine_angle  <- ((angle + 90) %% 180)
  angle        <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Should the spine start at the bottom left or the bottom right?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (spine_angle < 90) {
    spine_origin <- c(xoffset, yoffset)
  } else {
    spine_origin <- c(1 + xoffset, yoffset)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert angles to radians
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_angle <- stripe_angle * pi/180
  spine_angle  <- spine_angle  * pi/180

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # aspect ratio will determine maximum line length
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (aspect_ratio < 1) {
    ll <- 1/aspect_ratio
  } else {
    ll <- aspect_ratio
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hypotenuse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- sqrt(ll*ll + 1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We are going to create stripes that begin and end outside the viewport
  # no matter what the angle.  Make them long, and then we'll truncate them
  # later when we intersect with the area to be shaded
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rib_offset <- c(ll * cos(stripe_angle), ll * aspect_ratio * sin(stripe_angle))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a spine. As we walk along the spine we'll extend stripes out
  # at right angles.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spine_end   <- c(spine_origin[1] + ll * cos(spine_angle),
                   spine_origin[2] + ll * aspect_ratio * sin(spine_angle))

  Nribs <- ll/spacing

  spine_mat <- cbind(
    seq(spine_origin[1], spine_end[1], length.out = Nribs),
    seq(spine_origin[2], spine_end[2], length.out = Nribs)
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # One edge of the stripe is positioned x% of the way along, where this
  # percent is controlled by the pattern_density
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spine_offset  <- c(density * spacing * cos(spine_angle),
                     density * spacing * sin(spine_angle) * aspect_ratio)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We have enough now to construct a polygon for each stripe element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_corner_a <- t(t(spine_mat) + rib_offset)
  stripe_corner_b <- t(t(spine_mat) - rib_offset)
  stripe_corner_c <- t(t(spine_mat) - rib_offset + spine_offset)
  stripe_corner_d <- t(t(spine_mat) + rib_offset + spine_offset)

  stripe_coords_full <- lapply(seq(nrow(stripe_corner_a)), function(i) {
    rbind(
      stripe_corner_a[i,],
      stripe_corner_b[i,],
      stripe_corner_c[i,],
      stripe_corner_d[i,],
      stripe_corner_a[i,]
    )
  })

  stripe_coords <- lapply(stripe_coords_full, function(x) {list(x)})
  stripes_as_sfg_multipolygon <- sf::st_multipolygon(stripe_coords)

  stripes_as_sfg_multipolygon
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname create_pattern_none
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_stripes_via_sf <- function(params, boundary_df, aspect_ratio,
                                          legend = FALSE) {

  stopifnot(is_polygon_df(boundary_df))

  stripes_sf <- create_stripes_sf(
    angle   = params$pattern_angle,
    spacing = params$pattern_spacing,
    density = params$pattern_density,
    xoffset = params$pattern_xoffset,
    yoffset = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)

  striped_area    <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons <- convert_polygon_sf_to_polygon_df(striped_area)

  if (is.null(stripe_polygons)) {
    return(grid::nullGrob())
  }

  stripes_grob <- grid::polygonGrob(
    x = unit(stripe_polygons$x, "npc"),
    y = unit(stripe_polygons$y, "npc"),
    id = stripe_polygons$id,
    gp = gpar(
      col     = scales::alpha(params$pattern_colour, params$pattern_alpha),
      fill    = scales::alpha(params$pattern_fill  , params$pattern_alpha),
      lwd     = params$pattern_size * .pt,
      lty     = params$pattern_linetype,
      lineend = 'square'
    )
  )

  stripes_grob
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  library(ggplot2)
  library(dplyr)

  plot_df <- mpg %>% filter(manufacturer %in% c('lincoln', 'mercury', 'audi'))
  # plot_df <- mpg %>% filter(manufacturer %in% c('mercury'))

  p <- ggplot(plot_df, aes(x = manufacturer)) +
    geom_bar_pattern(
      aes(
        pattern         = manufacturer,
        pattern_fill    = manufacturer
      ),
      fill            = 'white',
      colour          = 'black',
      pattern_density = 0.3,
      pattern_spacing = 0.01,
      pattern_alpha   = 0.3,
      pattern_colour  = NA,
      pattern_angle   = 60,
      # pattern_xoffset = 0.07,
      pattern_key_scale_factor = 1.2
    ) +
  theme_bw() +
    labs(title = "ggpattern::geom_bar_pattern()") +
    scale_pattern_density_discrete() +
    scale_pattern_manual(values = c(lincoln = 'stripe', mercury = 'stripe', audi = 'stripe')) +
    theme(
      # legend.position = 'none',
      legend.justification = c(1, 1),
      legend.position = c(1, 1),
      legend.key.size = unit(3, 'cm')
    ) +
    coord_fixed(ratio = 1/8) +
    NULL

  system.time({
    print(p)
  })


  system.time({
    pdf("working/test-sf.pdf")
    print(p)
    dev.off()
  })


}








