test_that("`geom_map_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    skip_if_not_installed("mapproj")
    skip_if_not_installed("maps")
    expect_doppelganger("map", {
      library(maps)

      crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

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
