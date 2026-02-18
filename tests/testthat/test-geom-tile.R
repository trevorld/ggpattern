test_that("`geom_tile_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("tile", {
        df <- data.frame(
          x = rep(c(2, 5, 7, 9, 12), 2),
          y = rep(c(1, 2), each = 5),
          z = factor(rep(1:5, each = 2)),
          w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
        )

        ggplot(df, aes(x, y)) +
          geom_tile_pattern(
            aes(
              fill    = z,
              pattern = z
            ),
            colour = "grey50"
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_tile_pattern()") +
          theme(
            legend.position = 'bottom',
            legend.key.size = unit(1.5, 'cm')
          ) +
          coord_fixed(ratio = 4)
    })
})
