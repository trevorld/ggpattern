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

test_that("each map region gets the pattern of its row", {
    # region "b" (group 2) is listed before region "a" (group 1):
    # in non-linear coords `coord_munch()` sorts the polygons by group
    map <- data.frame(id = rep(c("b", "a"), each = 4), group = rep(c(2, 1), each = 4),
                      x = c(2, 3, 3, 2, 0, 1, 1, 0), y = c(0, 0, 1, 1, 0, 0, 1, 1))
    df <- data.frame(region = c("a", "b"), pat = c("circle", "stripe"), fill = c("red", "blue"))
    p <- ggplot(df, aes(map_id = region, pattern = I(pat), fill = I(fill))) +
        geom_map_pattern(map = map) +
        expand_limits(x = map$x, y = map$y)
    for (coord in list(coord_cartesian(), coord_polar())) {
        grob <- layer_grob(p + coord)[[1]]
        areas <- grob$children[grep("^area", names(grob$children))]
        # each area's pattern matches the region of its fill colour
        fills <- vapply(areas, function(a) a$children$fill$gp$fill, character(1L), USE.NAMES = FALSE)
        expect_equal(pattern_names(grob), unname(c("#FF0000" = "circle", "#0000FF" = "stripe")[fills]))
    }
})
