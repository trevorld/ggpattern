test_that("`geom_point_pattern()` patterns each filled point with its row's pattern", {
    skip_if(getRversion() < "4.2.0")
    df <- data.frame(x = 1:4, y = 1:4, shape = c(21, 19, 25, 22),
                     pattern = c("circle", "stripe", "gradient", "none"))
    p <- ggplot(df, aes(x, y, shape = I(shape), pattern = I(pattern))) + geom_point_pattern()
    grob <- layer_grob(p)[[1]]
    # (the `pch = 19` point has no area to fill)
    expect_equal(pattern_names(grob), c("circle", "gradient", "none"))
    # each point (in row order) is drawn in turn so overlapping points are drawn on top of each other
    expect_length(grob$children, nrow(df))
    expect_s3_class(grob$children[[2L]], "points")
    expect_equal(grob$children[[2L]]$pch, 19)

    # no filled points: just `GeomPoint`'s grob
    p <- ggplot(df, aes(x, y)) + geom_point_pattern(shape = 19)
    expect_s3_class(layer_grob(p)[[1]], "points")
})

test_that("`draw_key_point_pattern()` patterns filled points", {
    skip_if(getRversion() < "4.2.0")
    data <- data.frame(colour = "black", fill = "white", size = 5, stroke = 0.5, alpha = NA,
                       shape = 21, pattern = "circle")
    withr::local_pdf(NULL)
    key <- draw_key_point_pattern(data, list(), c(6, 6))
    expect_equal(pattern_names(key), "circle")
    data$shape <- 19
    expect_length(find_pattern_grobs(draw_key_point_pattern(data, list(), c(6, 6))), 0L)
})

test_that("`geom_point_pattern()` works as expected", {
    skip_if(getRversion() < "4.2.0")
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("magick")
    skip_if_not_installed("png")
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("point-image", {
        filenames <- c(system.file("img", "Rlogo.png", package = "png"),
                       system.file("img", "magpie.jpg", package = "ggpattern"),
                       system.file("img", "bug.jpg", package = "ggpattern"))
        df <- data.frame(x = c(1, 2, 3, 4), y = c(1, 3, 2, 4), shape = c(21, 21, 22, 19),
                         filename = filenames[c(1, 2, 3, 1)])
        ggplot(df, aes(x, y, shape = I(shape), pattern_filename = I(filename))) +
            geom_point_pattern(pattern = "image", pattern_type = "expand", size = 25, fill = "white") +
            lims(x = c(0.5, 4.5), y = c(0.5, 4.5)) +
            labs(title = "ggpattern::geom_point_pattern()")
    })

    expect_doppelganger("point-overlap", {
        df <- data.frame(x = c(1, 1.07, 1.14), y = 1, k = c("a", "b", "c"))
        ggplot(df, aes(x, y, pattern_fill = k)) +
            geom_point_pattern(size = 30, fill = "white", stroke = 2, pattern_density = 0.4) +
            lims(x = c(0.8, 1.5))
    })
})
