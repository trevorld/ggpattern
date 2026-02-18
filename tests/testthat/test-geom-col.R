test_that("`geom_col_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("col", {
        df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
        ggplot(df, aes(trt, outcome)) +
          geom_col_pattern(
            aes(
              pattern = trt,
              fill    = trt
            ),
            colour                   = 'black',
            pattern_density          = 0.5,
            pattern_key_scale_factor = 1.11
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_col_pattern()") +
          theme(legend.position = 'none') +
          coord_fixed(ratio = 1/2)
    })
})
