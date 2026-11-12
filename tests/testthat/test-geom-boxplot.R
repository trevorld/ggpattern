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

test_that("only the box of a boxplot gets a pattern", {
    # the outliers, whiskers, and median of a boxplot are left alone
    n_class <- length(unique(ggplot2::mpg$class))
    p <- ggplot(ggplot2::mpg, aes(class, hwy, pattern = class)) + geom_boxplot_pattern()
    grob <- layer_grob(p)[[1]]
    expect_length(find_pattern_grobs(grob), n_class)
    expect_true("points" %in% grob_classes(grob))

    # even filled outliers (pch 21 to 25) aren't patterned: only the box is
    p <- ggplot(ggplot2::mpg, aes(class, hwy)) +
        geom_boxplot_pattern(outlier.shape = 21, pattern = "circle")
    grob <- layer_grob(p)[[1]]
    expect_true("points" %in% grob_classes(grob))
    expect_length(find_pattern_grobs(grob), n_class)
})
