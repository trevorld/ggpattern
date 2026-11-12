test_that("`geom_area_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("area", {
        huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
        ggplot(huron, aes(year)) +
            geom_area_pattern(aes(y = level))
    })
})

test_that("`geom_ribbon_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("ribbon", {
        huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))

        ggplot(huron, aes(year)) +
          geom_ribbon_pattern(
              aes(
                ymin = level - 1,
                ymax = level + 1
              ),
              fill            = NA,
              colour          = 'black',
              pattern         = 'circle',
              pattern_spacing = 0.03,
              pattern_density = 0.5,
              pattern_angle   = 30,
              outline.type    = 'full'
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_ribbon_pattern()")
    })
})

test_that("each piece of a ribbon gets its pattern", {
    # a ribbon broken by missing values is one area per run of values with the same pattern
    df <- data.frame(x = 1:5, ymin = 0, ymax = c(1, 2, NA, 2, 1))
    p <- ggplot(df, aes(x, ymin = ymin, ymax = ymax)) + geom_ribbon_pattern(pattern = "circle")
    expect_equal(pattern_names(suppressWarnings(layer_grob(p))[[1]]), c("circle", "circle"))
    expect_error(
        layer_grob(ggplot(df, aes(x, ymin = 0, ymax = 1, pattern_angle = x)) + geom_ribbon_pattern()),
        "Pattern aesthetics can not vary along a ribbon"
    )

    # flipped ribbons with `na.rm = TRUE` (as `geom_ribbon()`)
    dfy <- data.frame(y = 1:5, xmin = 0, xmax = c(1, 2, NA, 2, 1))
    p <- ggplot(dfy, aes(y = y, xmin = xmin, xmax = xmax)) +
        geom_ribbon_pattern(na.rm = TRUE, orientation = "y", pattern = "circle")
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("circle", "circle"))
})
