# In `.Rbuildignore` so `{treemapify}` needn't be in `Suggests` (hence only run from a source checkout)
test_that("`make_GeomPattern()` works with a geom from another package", {
    skip_if(getRversion() < "4.2.0")
    skip_if_not_installed("treemapify")
    GeomTreemapPattern <- make_GeomPattern(treemapify::GeomTreemap)
    # (its `draw_panel()` method names its panel parameters `panel_scales` (as `{ggplot2}` (< 3.0.0) did)
    #  which our draw method calls `panel_params` so `make_constructor()` leaves it out)
    expect_false("panel_scales" %in% GeomTreemapPattern$parameters())
    expect_no_warning(geom_treemap_pattern <- make_constructor(GeomTreemapPattern))
    # it draws its tiles from the largest to the smallest (not in row order)
    df <- data.frame(area = c(1, 5, 2), k = c("a", "b", "c"))
    p <- ggplot(df, aes(area = area, pattern = k, fill = k)) + geom_treemap_pattern() +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch")) +
        scale_fill_manual(values = c("red", "green", "blue"))
    grob <- layer_grob(p)[[1]]
    expect_equal(pattern_names(grob), c("circle", "crosshatch", "stripe"))
    expect_colours(area_fills(grob), c("green", "blue", "red"))
})
