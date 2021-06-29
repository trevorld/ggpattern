context("geom")
test_that("geometry patterns work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("sf", {
      nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
      indices <- which(nc$CNTY_ID >= 1820 & nc$CNTY_ID <= 1830)
      ggplot(nc[indices, ]) +
        geom_sf_pattern(aes(pattern_spacing = AREA, pattern = NAME), fill = 'white') +
        theme_bw() +
        labs(title = "ggpattern::geom_sf()")
    })

    skip_if_not_installed("maps")
    expect_doppelganger("map", {
      library(maps)

      crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
      crimesm <- reshape2::melt(crimes, id = 1)

      states_map <- map_data("state")
      ggplot(crimes, aes(map_id = state)) +
        geom_map_pattern(
          aes(
            # fill            = Murder,
            pattern_spacing = state,
            pattern_density = state,
            pattern_angle   = state,
            pattern         = state
          ),
          fill = 'white',
          map = states_map
        ) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        coord_map() +
        theme_bw() +
        labs(title = "ggpattern::geom_map_pattern()")
    })
})
