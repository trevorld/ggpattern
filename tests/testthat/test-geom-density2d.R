test_that("`geom_density_2d_filled_pattern()` patterns each band with its pattern", {
    skip_if_not_installed("MASS")
    p <- ggplot(datasets::faithful, aes(waiting, eruptions)) +
        geom_density_2d_filled_pattern(aes(pattern = after_stat(level)), bins = 3) +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    expect_s3_class(p$layers[[1L]]$geom, c("GeomDensity2dFilledPattern", "GeomPolygonPattern"))
    expect_s3_class(p$layers[[1L]]$stat, "StatDensity2dFilled")
    expect_equal(pattern_names(layer_grob(p)[[1L]]), c("stripe", "circle", "crosshatch"))
    expect_identical(geom_density2d_filled_pattern, geom_density_2d_filled_pattern)
    expect_named(formals(geom_density_2d_filled_pattern), names(formals(ggplot2::geom_density_2d_filled)))
})
