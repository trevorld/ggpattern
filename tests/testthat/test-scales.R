test_that("scale errors work as expected", {
    expect_error(scale_pattern_linetype_continuous())
    expect_error(scale_pattern_shape_continuous())

})

test_that("scale warns work as expected", {
    expect_warning(scale_pattern_shape_ordinal())
    expect_warning(scale_pattern_alpha_discrete())
})

test_that("scales work as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    df <- data.frame(trt = c("a", "b", "c"),
                     outcome = c(2.3, 1.9, 3.2))
    gg <- ggplot(df, aes(trt, outcome)) + theme_bw()

    expect_doppelganger("fill_brewer_fill2_grey", {
        gg + geom_col_pattern(aes(pattern_fill=trt, pattern_fill2=trt, pattern_colour=trt),
                              fill = NA, colour='black', pattern='crosshatch') +
            scale_pattern_fill_brewer() +
            scale_pattern_fill2_grey() +
            scale_pattern_colour_brewer()
    })

    expect_doppelganger("fill2_brewer_fill_grey", {
        gg + geom_col_pattern(aes(pattern_fill=trt, pattern_fill2=trt, pattern_colour=trt),
                              fill = NA, colour='black', pattern='crosshatch') +
            scale_pattern_fill2_brewer() +
            scale_pattern_fill_grey() +
            scale_pattern_colour_grey()
    })

    expect_doppelganger("viridis_d", {
        gg + geom_col_pattern(aes(pattern_fill=trt, pattern_fill2=trt, pattern_colour=trt),
                              fill = NA, colour='black', pattern='crosshatch') +
            scale_pattern_fill2_viridis_d() +
            scale_pattern_fill_viridis_d() +
            scale_pattern_colour_viridis_d()
    })

    expect_doppelganger("shape", {
        gg + geom_col_pattern(aes(pattern_linetype=trt, pattern_shape=trt, pattern_colour=trt),
                              fill=NA, pattern='pch', pattern_density=0.5) +
            scale_pattern_colour_brewer() +
            scale_pattern_linetype() +
            scale_pattern_shape()
    })
})
