test_that("`geom_dotplot_pattern()` patterns each dot with its row's pattern when drawn", {
    skip_if(getRversion() < "4.2.0")
    df <- data.frame(x = c(1, 1, 2, 3), g = c("a", "a", "b", "c"))
    p <- ggplot(df, aes(x, pattern = g)) + geom_dotplot_pattern(binwidth = 0.5) +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    expect_s3_class(p$layers[[1L]]$geom, c("GeomDotplotPattern", "GeomDotplot"))
    expect_s3_class(p$layers[[1L]]$stat, "StatBindot")
    # the dots are only made (and so filled) when the grob is drawn
    grob <- layer_grob(p)[[1L]]
    expect_length(find_pattern_grobs(grob), 0L)
    withr::local_pdf(NULL)
    expect_equal(pattern_names(grid::forceGrob(grob)), c("stripe", "stripe", "circle", "crosshatch"))
    expect_no_error(print(p))

    expect_named(formals(geom_dotplot_pattern), names(formals(ggplot2::geom_dotplot)))
    expect_setequal(names(GeomDotplotPattern$default_aes),
                    c(names(ggplot2::GeomDotplot$default_aes), names(default_pattern_aes())))
    expect_message(geom_dotplot_pattern(position = "stack"), "doesn't work properly")
    expect_message(geom_dotplot_pattern(stackgroups = TRUE), "binpositions")
    expect_error(geom_dotplot_pattern(stackdir = "sideways"), "stackdir")

    # the key's dot is filled with the key's pattern
    data <- data.frame(colour = "black", fill = "white", linetype = 1, alpha = NA, pattern = "circle")
    expect_equal(pattern_names(GeomDotplotPattern$draw_key(data, list(), c(6, 6))), "circle")
})
