context("geometry")
test_that("geometry patterns work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    df <- data.frame(trt = c("a", "b", "c"),
                     outcome = c(2.3, 1.9, 3.2))

    pattern_ggplot <- function(pattern) {
        ggplot(df, aes(trt, outcome)) +
            geom_col_pattern(aes(fill=trt),
                             colour='black',
                             pattern = pattern) +
            theme_bw() +
            labs(title = pattern)
    }

    expect_doppelganger("circle", pattern_ggplot("circle"))
    expect_doppelganger("crosshatch", pattern_ggplot("crosshatch"))
    expect_doppelganger("none", pattern_ggplot("none"))
    expect_doppelganger("stripe", pattern_ggplot("stripe"))
})
