test_that("our pattern geoms and their constructors match their `{ggplot2}` counterparts", {
    # Our pattern geoms copy the aesthetics and the draw method arguments of their `{ggplot2}` counterpart
    # and their constructors its arguments so this fails when `{ggplot2}` changes them
    # (i.e. we should update ours): not a bug for users of the released package so skipped on CRAN
    # (but run by our scheduled check against the development `{ggplot2}`).
    #
    # When it fails against the development `{ggplot2}` (whose version ends in `.9000`):
    # 1. Update ours to match (in a way that still works with the released `{ggplot2}`
    #    e.g. a draw method that passes on whichever argument the installed `{ggplot2}`'s method has).
    # 2. Skip this test for earlier versions (which ours no longer match)
    #    e.g. `skip_if(packageVersion("ggplot2") < "4.0.3.9000")`.
    # 3. Once that `{ggplot2}` is released bump the `{ggplot2}` version we import and remove the skip.
    skip_on_cran()
    ggplot2_exports <- getNamespaceExports("ggplot2")
    draw_args <- function(method) setdiff(names(formals(ggproto_fn(method))), "self")

    geoms <- sort(grep("^Geom.+Pattern$", getNamespaceExports("ggpattern"), value = TRUE))
    expect_in(sub("Pattern$", "", geoms), ggplot2_exports)
    for (name in geoms) {
        ours <- getExportedValue("ggpattern", name)
        theirs <- getExportedValue("ggplot2", sub("Pattern$", "", name))
        expect_setequal(names(ours$default_aes), c(names(theirs$default_aes), names(default_pattern_aes())))
        for (method in c("draw_panel", "draw_group")) {
            expect_equal(draw_args(ours[[method]]), draw_args(theirs[[method]]), info = paste(name, method))
        }
        for (field in c("required_aes", "optional_aes", "non_missing_aes", "rename_size")) {
            expect_equal(ours[[field]], theirs[[field]], info = paste(name, field))
        }
    }

    constructors <- sort(grep("^geom_.+_pattern$", getNamespaceExports("ggpattern"), value = TRUE))
    expect_in(sub("_pattern$", "", constructors), ggplot2_exports)
    for (name in constructors) {
        ours <- getExportedValue("ggpattern", name)
        theirs <- getExportedValue("ggplot2", sub("_pattern$", "", name))
        expect_equal(names(formals(ours)), names(formals(theirs)), info = name)
    }
})

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
