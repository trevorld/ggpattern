test_that("`geom_contour_filled_pattern()` patterns each band with its pattern", {
    p <- ggplot(ggplot2::faithfuld, aes(waiting, eruptions, z = density)) +
        geom_contour_filled_pattern(aes(pattern = after_stat(level)), bins = 3) +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    expect_s3_class(p$layers[[1L]]$geom, c("GeomContourFilledPattern", "GeomPolygonPattern"))
    expect_s3_class(p$layers[[1L]]$stat, "StatContourFilled")
    expect_equal(pattern_names(layer_grob(p)[[1L]]), c("stripe", "circle", "crosshatch"))
    expect_named(formals(geom_contour_filled_pattern), names(formals(ggplot2::geom_contour_filled)))
})
