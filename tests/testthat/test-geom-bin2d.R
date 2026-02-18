test_that("`geom_bin2d_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    suppressWarnings( # Removed 478 rows containing non-finite values (stat_bin2d)
    expect_doppelganger("bin2d", {
        ggplot(ggplot2::diamonds, aes(x, y)) +
            xlim(4, 10) + ylim(4, 10) +
            geom_bin2d_pattern(aes(pattern_spacing = ..density..),
                               fill = 'white', bins = 6, colour = 'black', linewidth = 1) +
            theme_bw(18) +
            theme(legend.position = 'none') +
            labs(title = "ggpattern::geom_bin2d_pattern()")
    }))
})
