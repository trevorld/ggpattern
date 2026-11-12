test_that("`geom_sf_pattern()` works as expected", {
    skip_on_ci()
    skip_if_not_installed("sf")
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("sf", {
      nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
      indices <- which(nc$CNTY_ID >= 1820 & nc$CNTY_ID <= 1830)
      nc <- nc[indices, ]

      p <- ggplot(nc) +
        geom_sf_pattern(
          aes(
            pattern = NAME,
            fill    = NAME
          ),
          pattern_aspect_ratio = 2.8
        ) +
        theme_bw(15) +
        theme(legend.key.size = unit(1.5, 'cm')) +
        labs(title = "ggpattern::geom_sf()")

      p
    })

    expect_doppelganger("sf-hole", {
      # coords <- read.csv(textConnection(
      # 'x,y,idx
      # 0,0,1
      # 4,0,1
      # 4,4,1
      # 0,4,1
      # 0,0,1
      # 1,1,2
      # 2,1,2
      # 2,2,2
      # 1,2,2
      # 1,1,2'))

      # this_sf <- coords %>%
      #   sfheaders::sf_polygon(x='x', y='y', linestring_id = 'idx')
      # Avoid {sfheaders} Suggests by using output from `dput(this_sf)`
      this_sf <- structure(list(id = 1L,
            geometry = structure(list(structure(list(
                                                     structure(c(0, 4, 4, 0, 0, 0, 0, 4, 4, 0), .Dim = c(5L, 2L)),
                                                     structure(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), .Dim = c(5L, 2L))),
                                                class = c("XY", "POLYGON", "sfg"))),
                                 n_empty = 0L,
                                 crs = structure(list(
                                                      input = NA_character_,
                                                      wkt = NA_character_),
                                                 class = "crs"),
                                 class = c("sfc_POLYGON", "sfc"),
                                 precision = 0,
                                 bbox = structure(c(xmin = 0, ymin = 0, xmax = 4, ymax = 4),
                                                  class = "bbox"))),
                           class = c("sf", "data.frame"),
                           sf_column = "geometry", row.names = 1L)

      ggplot(this_sf) +
        geom_sf_pattern(aes(geometry = geometry), fill = 'lightblue')
    })
})

test_that("`geom_sf_pattern()` draws one pattern per row", {
    skip_if_not_installed("sf")
    triangle <- function(x0) sf::st_polygon(list(rbind(c(x0, 0), c(x0 + 1, 0), c(x0 + 1, 1), c(x0, 0))))
    geometry <- sf::st_sfc(
        # a multipolygon (e.g. a county with islands) is a single area
        sf::st_multipolygon(list(triangle(0), triangle(2))),
        sf::st_polygon(),
        sf::st_linestring(rbind(c(0, 2), c(4, 2))),
        # the polygons of a geometry collection are each their own area
        sf::st_geometrycollection(list(triangle(4), sf::st_point(c(5, 5)), triangle(6))),
        triangle(8)
    )
    df <- sf::st_sf(id = c("a", "b", "c", "d", "e"), geometry = geometry)
    p <- ggplot(df) +
        geom_sf_pattern(aes(pattern = id), arrow = arrow(type = "closed"), arrow.fill = "red") +
        scale_pattern_manual(values = c(a = "circle", b = "none", c = "none", d = "gradient", e = "stripe"))
    grob <- layer_grob(p)[[1]]
    expect_equal(pattern_names(grob), c("circle", "gradient", "gradient", "stripe"))

    # the line (e.g. any arrow heads) is left alone
    lines <- find_grobs(grob, "lines")
    expect_length(lines, 1L)
    expect_equal(lines[[1L]]$gp$fill, "red")

    # a homogeneous multipolygon layer (drawn as a single `pathgrob`) is still one pattern per row
    nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[c(1:4, 47), ]
    p <- ggplot(nc) + geom_sf_pattern(aes(pattern = NAME)) +
        scale_pattern_manual(values = c("circle", "gradient", "stripe", "none", "crosshatch"))
    expect_length(pattern_names(layer_grob(p)[[1]]), nrow(nc)) # Currituck (row 5) has 3 polygons
})
