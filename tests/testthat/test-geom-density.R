test_that("`geom_density_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("density", {
        ggplot(mtcars) +
        geom_density_pattern(
          aes(
            x            = mpg,
            pattern_fill = as.factor(cyl),
            pattern      = as.factor(cyl)
          ),
          fill                     = 'white',
          pattern_key_scale_factor = 1.2,
          pattern_density          = 0.4
        ) +
        theme_bw(18) +
        labs(title = "ggpattern::geom_density_pattern()") +
        theme(legend.key.size = unit(2, 'cm')) +
        coord_fixed(ratio = 100)
    })
})
