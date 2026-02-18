test_that("`geom_crossbar_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("crossbar", {
        df <- data.frame(
             trt = factor(c(1, 1, 2, 2)),
             resp = c(1, 5, 3, 4),
             group = factor(c(1, 2, 1, 2)),
             upper = c(1.1, 5.3, 3.3, 4.2),
             lower = c(0.8, 4.6, 2.4, 3.6)
        )

        ggplot(df, aes(trt, resp, colour = group)) +
            geom_crossbar_pattern(
              aes(
                  ymin          = lower,
                  ymax          = upper,
                  pattern_angle = trt,
                  pattern       = group
                  ), width        = 0.2,
              pattern_spacing = 0.02
              ) +
            theme_bw(18) +
            labs(title = "ggpattern::geom_crossbar_pattern()") +
            theme(legend.key.size = unit(1.5, 'cm')) +
            coord_fixed(ratio = 1/3)
    })
})
