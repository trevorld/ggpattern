test_that("`geom_histogram_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("histogram", {
         ggplot(ggplot2::diamonds, aes(carat)) +
             geom_histogram_pattern(pattern = "stripe", bins = 30)
    })
})
