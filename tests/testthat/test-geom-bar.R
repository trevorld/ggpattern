test_that("`geom_bar_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("bar", {
        ggplot(ggplot2::mpg, aes(class)) +
          geom_bar_pattern(
            aes(
              pattern = class,
              pattern_angle = class
            ),
            fill            = 'white',
            colour          = 'black',
            pattern_spacing = 0.025
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_bar_pattern()") +
          theme(legend.position = 'none') +
          coord_fixed(ratio = 1/15) +
          scale_pattern_discrete(guide = guide_legend(nrow = 1))
    })
})
