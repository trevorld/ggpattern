test_that("lists of vectors aesthetics work as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")

    df <- data.frame(trt = c("a", "b", "c"),
                     outcome = c(2.3, 1.9, 3.2))
    vdiffr::expect_doppelganger("vectorized `pattern_fill`", {
        ggplot(df, aes(trt, outcome)) +
            geom_col_pattern(aes(fill=trt, pattern_fill=trt),
                             linewidth = 1,
                             pattern_linetype = list(1, 1, 1:3),
                             colour='black',
                             pattern = "stripe") +
            scale_pattern_fill_manual(values = list("white", "black", c("blue", "red", "white"))) +
            theme_bw() +
            labs(title = "vectorized `pattern_fill`")
    })
})

test_that("pattern fills work as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("svglite", "2.1.3")
    skip_if_not(getRversion() >= "4.2.0")
    write_svg_bleeding_edge <- function(plot, file, title = "") {
        svglite::svglite(file)
        print(plot)
        invisible(grDevices::dev.off())
    }
    df <- data.frame(trt = c("a", "b", "c"),
                     outcome = c(2.3, 1.9, 3.2))
    circle_fill <- gridpattern::patternFill("circle", fill = "white", 
                                            density = 0.4, spacing = 0.1)
    vdiffr::expect_doppelganger("pattern fill `pattern_fill`", {
        ggplot(df, aes(trt, outcome)) +
            geom_col_pattern(aes(fill=trt, pattern_fill=trt),
                             linewidth = 1,
                             colour='black',
                             pattern = "stripe", 
                             pattern_density = 0.6, 
                             pattern_spacing = 0.1) +
            scale_pattern_fill_manual(values = list(circle_fill, "black", "white")) +
            theme_bw() +
            labs(title = "pattern fill `pattern_fill`")
    }, writer = write_svg_bleeding_edge)

    vdiffr::expect_doppelganger("pattern fill `fill`", {
        ggplot(df, aes(trt, outcome)) +
            geom_col_pattern(aes(fill=trt, pattern_fill=trt),
                             linewidth = 1,
                             colour='black',
                             pattern = "stripe", 
                             pattern_density = 0.2, 
                             pattern_spacing = 0.1) +
            scale_fill_manual(values = list(circle_fill, "black", "white")) +
            theme_bw() +
            labs(title = "pattern fill `fill`")
    }, writer = write_svg_bleeding_edge)
})
