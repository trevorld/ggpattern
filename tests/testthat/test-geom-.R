test_that("zero-size legend keys don't draw a pattern", {

    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    p <- ggplot(df, aes(trt, outcome, pattern_fill = trt)) +
        geom_col_pattern() +
        theme(legend.key.size = unit(0, "mm"))

    withr::local_pdf(NULL)
    expect_no_error(print(p))
    expect_no_error(print(p + theme(legend.key.width = unit(5, "mm"))))
})

test_that("legend keys pattern the area of {ggplot2}'s key glyph", {
    data <- data.frame(colour = "black", fill = "white", linewidth = 0.5, linetype = 1, alpha = NA,
                       pattern = "circle", pattern_fill = "red")
    withr::local_pdf(NULL)
    grid::pushViewport(grid::viewport(width = unit(1, "cm"), height = unit(1, "cm")))

    key <- draw_key_polygon_pattern(data, list(), c(10, 10))
    patterns <- find_pattern_grobs(key)
    expect_length(patterns, 1L)
    expect_equal(patterns[[1L]]$pattern, "circle")
    expect_true(is.numeric(patterns[[1L]]$legend))
    # {ggplot2}'s size attributes are kept
    expect_equal(attr(key, "width"), attr(ggplot2::draw_key_polygon(data, list(), c(10, 10)), "width"))

    # only the box of the boxplot / crossbar keys (also when flipped)
    for (params in list(list(), list(flipped_aes = TRUE))) {
        expect_length(find_pattern_grobs(draw_key_boxplot_pattern(data, params, c(10, 10))), 1L)
        expect_length(find_pattern_grobs(draw_key_crossbar_pattern(data, params, c(10, 10))), 1L)
    }

    # a zero-size key is {ggplot2}'s key without a pattern
    expect_length(find_pattern_grobs(draw_key_polygon_pattern(data, list(), c(0, 0))), 0L)
})
