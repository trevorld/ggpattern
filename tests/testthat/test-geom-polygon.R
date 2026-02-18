test_that("`geom_polygon_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("polygon", {
      angle <- seq(0, 2*pi, length.out = 7) + pi/6
      polygon_df <- data.frame(
        angle = angle,
        x     = cos(angle),
        y     = sin(angle)
      )

      ggplot(polygon_df) +
        geom_polygon_pattern(
          aes(x = x, y = y),
          fill            = 'white',
          colour          = 'black',
          pattern_spacing = 0.15,
          pattern_density = 0.4,
          pattern_fill    = 'lightblue',
          pattern_colour  = '#002366',
          pattern_angle   = 45
        ) +
        labs(title = "ggpattern") +
        coord_equal() +
        theme_bw(25) +
        theme(axis.title = element_blank())
    })

    expect_doppelganger("polygon_hole", {
      # example modified from {ggplot2} unit tests
      tbl <- data.frame(
        x = c(
          0, 10, 10, 0,
          20, 30, 30, 20,
          22, 28, 28, 22
        ),
        y = c(
          0, 0, 10, 10,
          20, 20, 30, 30,
          22, 22, 28, 28
        ),
        group = c(rep(1, 4), rep(2, 8)),
        subgroup = c(rep(1, 8), rep(2, 4))
      )

      p <- ggplot(tbl, aes(x, y, group = group, subgroup = subgroup)) +
        geom_polygon_pattern(fill="lightblue", color="black")
    })
})
