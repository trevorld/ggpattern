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
