test_that("`geom_boxplot_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("boxplot", {
        ggplot(ggplot2::mpg, aes(class, hwy)) +
            geom_boxplot_pattern(
                 aes(
                     pattern      = class,
                     pattern_fill = class
                     ),
                 pattern_spacing = 0.03
                 ) +
            theme_bw(18) +
            labs(title = "ggpattern::geom_boxplot_pattern()") +
            theme(legend.key.size = unit(1.5, 'cm')) +
            coord_fixed(1/8)
    })
})
