test_that("`geom_hex_pattern()` patterns each hexagon with its row's pattern", {
    skip_if_not_installed("hexbin")
    df <- data.frame(x = c(3, 1, 2), y = c(1, 3, 2), pattern = c("circle", "stripe", "crosshatch"))
    p <- ggplot(df, aes(x, y, group = 1, pattern = I(pattern))) + geom_hex_pattern(stat = "identity")
    expect_s3_class(p$layers[[1L]]$geom, c("GeomHexPattern", "GeomHex"))
    expect_equal(pattern_names(layer_grob(p)[[1L]]), df$pattern)
    expect_s3_class(geom_hex_pattern()$stat, "StatBinhex")

    withr::local_pdf(NULL)
    data <- data.frame(colour = "black", fill = "white", linewidth = 0.5, linetype = 1, alpha = NA,
                       pattern = "circle")
    expect_equal(pattern_names(GeomHexPattern$draw_key(data, list(), c(6, 6))), "circle")
    p <- ggplot(ggplot2::diamonds, aes(carat, price)) + geom_hex_pattern(bins = 5)
    expect_no_error(print(p))
})
