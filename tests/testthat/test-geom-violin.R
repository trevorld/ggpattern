test_that("`geom_violin_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("violin", {
        ggplot(mtcars, aes(as.factor(cyl), mpg)) +
          geom_violin_pattern(aes(pattern = as.factor(cyl))) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_violin_pattern()") +
          theme(
            legend.key.size  = unit(2, 'cm')
          ) +
          coord_fixed(1/15)
    })
})
