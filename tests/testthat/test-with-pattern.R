test_that("`with_pattern_aes()` checks there is a row per area", {
    skip_if(getRversion() < "4.2.0")
    grob <- grid::rectGrob(x = c(0.25, 0.75), width = 0.4)
    params <- data.frame(pattern = c("stripe", "circle", "none"))
    expect_error(with_pattern_aes(grob, params), "one value per area")
    expect_equal(pattern_names(with_pattern_aes(grob, params[2:3, , drop = FALSE])), c("circle", "none"))
    # a single row is used for every area
    expect_equal(pattern_names(with_pattern_aes(grob, params[2L, , drop = FALSE])), c("circle", "circle"))
    # grobs without areas are left alone
    lines <- grid::linesGrob()
    expect_identical(with_pattern_aes(lines, params), lines)
})

test_that("`with_pattern_aes()` passes the other aesthetics on to the pattern", {
    skip_if(getRversion() < "4.2.0")
    captured <- local_capture_pattern("capture")
    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    p <- ggplot(df, aes(trt, outcome)) +
        geom_col_pattern(pattern = "capture", pattern_fill = "blue", fill = "red", colour = "green")
    params <- captured(p)
    expect_equal(params$pattern_fill, "blue")
    # e.g. a (custom) pattern may use `fill` as well as `pattern_fill`
    expect_equal(params$fill, "red")
    expect_equal(params$colour, "green")
    expect_true("group" %in% names(params))
    # but not the names `gridpattern::with_pattern()` reserves nor `PANEL`
    expect_false("PANEL" %in% names(params))

    grob <- grid::rectGrob()
    data <- data.frame(pattern = "circle", x = 1, y = 2, id = 3, name = "a", PANEL = factor(1), group = 1L)
    expect_equal(pattern_names(with_pattern_aes(grob, data)), "circle")
    # a list of aesthetics is fine too
    expect_equal(pattern_names(with_pattern_aes(grob, list(pattern = "circle"))), "circle")
})

test_that("`with_pattern_key()` fills the areas of a key glyph", {
    skip_if(getRversion() < "4.2.0")
    withr::local_pdf(NULL)
    data <- data.frame(fill = "white", colour = "black", linewidth = 0.5, linetype = 1, alpha = NA,
                       pattern = "circle")
    key <- ggplot2::draw_key_polygon(data, list(), c(6, 6))
    expect_equal(pattern_names(with_pattern_key(key, data, c(6, 6))), "circle")
    # nothing to draw in a zero-size key
    expect_identical(with_pattern_key(key, data, c(0, 0)), key)
})

test_that("`make_GeomPattern()` makes a pattern geom from a geom", {
    skip_if(getRversion() < "4.2.0")
    scale <- scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))

    # a geom with its own `draw_panel()` method drawing an area per row
    GeomColPattern2 <- make_GeomPattern(GeomCol, "GeomColPattern2")
    expect_s3_class(GeomColPattern2, c("GeomColPattern2", "GeomCol"))
    expect_true(all(names(pattern_aesthetics) %in% names(GeomColPattern2$default_aes)))
    expect_true(all(names(GeomCol$default_aes) %in% names(GeomColPattern2$default_aes)))
    expect_equal(GeomColPattern2$parameters(), GeomCol$parameters())
    geom_col_pattern2 <- make_constructor(GeomColPattern2, position = "stack")
    df <- data.frame(x = c("a", "b", "c"), y = 3:1)
    p <- ggplot(df, aes(x, y, pattern = x)) + geom_col_pattern2() + scale
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle", "crosshatch"))
    # its grob is named after the geom (like `{ggplot2}` names the grobs of its geoms)
    expect_match(layer_grob(p)[[1]]$name, "^geom_col_pattern2\\.")
    # (also that of a geom extending it or one made by tracing but not a `gList`)
    GeomColPattern3 <- ggproto("GeomColPattern3", GeomColPattern2)
    p3 <- ggplot(df, aes(x, y)) + layer(geom = GeomColPattern3, stat = "identity", position = "stack")
    expect_match(layer_grob(p3)[[1]]$name, "^geom_col_pattern3\\.")
    GeomColTraced <- make_GeomPattern(GeomCol, "GeomColTraced", area_by = "trace")
    p3 <- ggplot(df, aes(x, y)) + layer(geom = GeomColTraced, stat = "identity", position = "stack")
    expect_match(layer_grob(p3)[[1]]$name, "^geom_col_traced\\.")
    GeomList <- ggproto("GeomList", Geom, required_aes = "x", draw_key = draw_key_rect,
                        draw_panel = function(data, panel_params, coord) grid::gList(grid::rectGrob()))
    p3 <- ggplot(df[1L, ], aes(x)) + layer(geom = make_GeomPattern(GeomList), stat = "identity", position = "identity")
    expect_s3_class(layer_grob(p3)[[1]], "gList")
    expect_equal(snakeize("GeomDensity2dFilledPattern"), "geom_density2d_filled_pattern")
    # its key glyph is filled with the key's pattern
    withr::local_pdf(NULL)
    key <- GeomColPattern2$draw_key(data.frame(fill = "white", colour = "black", linewidth = 0.5,
                                                 linetype = 1, alpha = NA, pattern = "circle"),
                                      list(), c(6, 6))
    expect_equal(pattern_names(key), "circle")

    # a geom with its own `draw_group()` method (its outliers, whiskers, and median have no area)
    GeomBoxplotPattern2 <- make_GeomPattern(GeomBoxplot, "GeomBoxplotPattern2")
    expect_equal(GeomBoxplotPattern2$parameters(), GeomBoxplot$parameters())
    p <- ggplot(ggplot2::mpg, aes(drv, hwy, pattern = drv)) +
        layer(geom = GeomBoxplotPattern2, stat = "boxplot", position = "dodge2") + scale
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle", "crosshatch"))

    # a geom drawing an area per group
    GeomPolygonPattern2 <- make_GeomPattern(GeomPolygon, "GeomPolygonPattern2", area_by = "group")
    df <- data.frame(x = c(0, 1, 1, 2, 3, 3), y = c(0, 0, 1, 0, 0, 1), g = rep(c("b", "a"), each = 3))
    p <- ggplot(df, aes(x, y, group = g, pattern = g)) +
        layer(geom = GeomPolygonPattern2, stat = "identity", position = "identity") + scale
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle"))

    # the draw method's parameters after its `...` are still passed on by name
    GeomViolinPattern2 <- make_GeomPattern(GeomViolin, "GeomViolinPattern2")
    p <- ggplot(ggplot2::mpg, aes(drv, hwy)) +
        layer(geom = GeomViolinPattern2, stat = "ydensity", position = "dodge",
              params = list(quantiles = 0.5, quantile_gp = list(linetype = 3, colour = "red")))
    quantiles <- layer_grob(p)[[1]]$children[[1L]]$children[[2L]]
    expect_equal(quantiles$gp$lty, 3)
    expect_colours(quantiles$gp$col, "red")

    expect_equal(class(make_GeomPattern(GeomTile))[1L], "GeomTilePattern")
    expect_error(make_GeomPattern(geom_tile()), "must be a <Geom> object")
})

test_that("`area_by` functions are given the draw method's parameters they have arguments for", {
    GeomRev <- ggproto("GeomRev", GeomRect,
        draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre", rev = FALSE) {
            if (rev) data <- data[rev(seq_len(nrow(data))), ]
            ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord, lineend = lineend, linejoin = linejoin)
        }
    )
    df <- data.frame(x = 1:3, k = c("a", "b", "c"))
    plot <- function(geom) {
        ggplot(df, aes(xmin = x, xmax = x + 0.9, ymin = 0, ymax = 1, pattern = k)) +
            layer(geom = geom, stat = "identity", position = "identity", params = list(rev = TRUE)) +
            scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    }
    by_rev <- function(data, rev) if (rev) data[rev(seq_len(nrow(data))), ] else data
    p <- plot(make_GeomPattern(GeomRev, area_by = by_rev))
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("crosshatch", "circle", "stripe"))
    # (or all of them with `...`)
    by_dots <- function(data, ...) by_rev(data, list(...)$rev)
    p <- plot(make_GeomPattern(GeomRev, area_by = by_dots))
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("crosshatch", "circle", "stripe"))
    # (and none if it has no arguments for them)
    p <- plot(make_GeomPattern(GeomRev, area_by = function(data) data))
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle", "crosshatch"))
})

test_that("`make_GeomPattern()` knows the `area_by` of `{ggplot2}` geoms", {
    expect_equal(known_area_by(GeomRect), "row")
    expect_equal(known_area_by(GeomCol), "row")
    expect_equal(known_area_by(GeomPolygon), "group")
    expect_equal(known_area_by(GeomArea), "group")
    # a geom that draws like one we know (e.g. a layer's geom with its own key glyph)
    expect_equal(known_area_by(ggproto(NULL, GeomRect, draw_key = draw_key_point)), "row")
    # but not one with its own draw method nor a geom we don't know
    GeomRect2 <- ggproto("GeomRect2", GeomRect, draw_panel = function(self, data, panel_params, coord) NULL)
    expect_equal(known_area_by(GeomRect2), "trace")
    expect_equal(known_area_by(GeomSf), "trace")
    expect_error(make_GeomPattern(GeomRect, area_by = "rows"), "`area_by` must be one of")

    # a geom that doesn't draw its areas the way its `area_by` says
    GeomRectPattern3 <- make_GeomPattern(GeomRect, "GeomRectPattern3", area_by = "group")
    df <- data.frame(xmin = 1:3, xmax = 2:4, ymin = 0, ymax = 1, k = c("a", "a", "b"), g = c(1, 1, 2))
    p <- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, pattern = k, group = g)) +
        layer(geom = GeomRectPattern3, stat = "identity", position = "identity")
    expect_error(layer_grob(p), "drew 3 areas but `area_by = \"group\"` gave 2 rows",
                 class = "ggpattern_area_mismatch")
    # (with the error of `{gridpattern}` as its parent)
    expect_error(layer_grob(p), class = "gridpattern_area_mismatch")

    skip_if(getRversion() < "4.2.0")
    # so e.g. the pattern aesthetics can't vary along a ribbon
    GeomRibbonPattern2 <- make_GeomPattern(GeomRibbon, "GeomRibbonPattern2")
    df <- data.frame(x = 1:4)
    p <- ggplot(df, aes(x, ymin = 0, ymax = x)) +
        layer(geom = GeomRibbonPattern2, stat = "identity", position = "identity", params = list(pattern = "circle"))
    expect_equal(pattern_names(layer_grob(p)[[1]]), "circle")
    p <- ggplot(df, aes(x, ymin = 0, ymax = x, pattern_angle = x)) +
        layer(geom = GeomRibbonPattern2, stat = "identity", position = "identity")
    expect_error(layer_grob(p), "Pattern aesthetics can not vary within an area")
})

test_that("the areas `{ggplot2}`'s geoms draw have the rows we assume they do", {
    # Tracing finds the row of each area from the fill the geom drew it with
    # so (unlike the `area_by` we know for a geom or the rows our own pattern geoms pick)
    # it doesn't assume anything about the order the geom draws its areas in.
    # Not skipped on CRAN so that it is also run by the reverse dependency checks of `{ggplot2}`.
    skip_if(getRversion() < "4.2.0")
    withr::local_pdf(NULL)
    pats <- c("stripe", "circle", "crosshatch", "weave")
    # (rows that aren't in the order of their positions nor of their groups)
    rows <- data.frame(x = c(3, 1, 4, 2), y = c(2, 4, 1, 3), pat = pats, g = c("c", "a", "d", "b"),
                       shape = c(21, 19, 22, 24))
    groups <- data.frame(x = c(5, 6, 5, 6, 1, 2, 1, 2, 3, 4, 3, 4), y = c(1, 2, 2, 3, 3, 4, 4, 5, 2, 3, 3, 4),
                         g = rep(c("c", "a", "b"), each = 4L), pat = rep(pats[1:3], each = 4L))
    polygons <- data.frame(x = c(5, 6, 5, 1, 2, 1, 3, 4, 3), y = rep(c(0, 0, 1), 3L),
                           g = rep(c("c", "a", "b"), each = 3L), pat = rep(pats[1:3], each = 3L))
    values <- data.frame(g = c("b", "c", "a"), pat = pats[1:3])
    level_pattern <- aes(waiting, eruptions, z = density, pattern = after_stat(pats[as.integer(level)]))
    # how to draw a layer of each `{ggplot2}` geom whose areas have different patterns
    spec <- function(data, mapping, stat = "identity", position = "identity", params = list(),
                     patterns = unique(data$pat), extra = NULL) {
        list(data = data, mapping = mapping, stat = stat, position = position, params = params,
             patterns = patterns, extra = extra)
    }
    specs <- list(
        GeomRect = spec(rows, aes(xmin = x, xmax = x + 0.9, ymin = 0, ymax = y, pattern = pat)),
        GeomTile = spec(rows, aes(x, y, pattern = pat)),
        GeomBar = spec(rows, aes(x, y, pattern = pat)),
        GeomCol = spec(rows, aes(x, y, pattern = pat)),
        GeomBin2d = spec(rows, aes(x, y, pattern = pat)),
        GeomCrossbar = spec(rows, aes(x, y, ymin = y - 1, ymax = y + 1, pattern = pat)),
        GeomBoxplot = spec(rows, aes(x, ymin = y - 2, lower = y - 1, middle = y, upper = y + 1, ymax = y + 2,
                                     group = g, pattern = pat)),
        # (a single group so that its `draw_group()` method is given all of the rows
        #  with a pattern per bin since the stat drops the aesthetics that vary within a group)
        GeomDotplot = spec(rows, aes(x, group = 1, pattern = after_stat(pats[seq_along(count)])), stat = "bindot",
                           params = list(binwidth = 0.5)),
        GeomLabel = spec(rows, aes(x, y, label = g, pattern = pat)),
        # (only the filled symbols have an area)
        GeomPoint = spec(rows, aes(x, y, shape = I(shape), pattern = pat), params = list(size = 10),
                         patterns = pats[-2L]),
        GeomPolygon = spec(polygons, aes(x, y, group = g, pattern = pat)),
        GeomMap = spec(values, aes(map_id = g, pattern = pat),
                       params = list(map = data.frame(x = polygons$x, y = polygons$y, id = polygons$g)),
                       extra = expand_limits(x = polygons$x, y = polygons$y)),
        GeomContourFilled = spec(ggplot2::faithfuld, level_pattern, stat = "contour_filled", params = list(bins = 4),
                                 patterns = pats),
        GeomDensity2dFilled = spec(ggplot2::faithfuld, level_pattern, stat = "contour_filled", params = list(bins = 4),
                                   patterns = pats),
        GeomRibbon = spec(groups, aes(x, ymin = 0, ymax = y, group = g, pattern = pat)),
        GeomArea = spec(groups, aes(x, y, group = g, pattern = pat), stat = "align", position = "stack"),
        GeomDensity = spec(groups, aes(y, group = g, pattern = pat), stat = "density"),
        GeomViolin = spec(groups, aes(g, y, pattern = pat), stat = "ydensity")
    )
    # (the specs of geoms that need a suggested package)
    suggested <- c(GeomHex = "hexbin", GeomSf = "sf")
    if (requireNamespace("hexbin", quietly = TRUE)) {
        # (a single group so that its `draw_group()` method is given all of the rows)
        specs$GeomHex <- spec(rows, aes(x, y, group = 1, pattern = pat))
    }
    if (requireNamespace("sf", quietly = TRUE)) {
        square <- function(x) sf::st_polygon(list(cbind(x + c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
        # (the point doesn't have an area and the squares of the multipolygon are filled as one area)
        geometry <- sf::st_sfc(square(6), sf::st_multipolygon(list(square(0), square(2))),
                               sf::st_point(c(9, 0)), square(4))
        specs$GeomSf <- spec(sf::st_sf(pat = pats, geometry = geometry), aes(geometry = geometry, pattern = pat),
                             stat = "sf", patterns = pats[-3L], extra = coord_sf())
    }
    # the patterns of the areas (in drawing order) `geom` draws for a `spec`
    area_patterns <- function(geom, spec) {
        p <- ggplot(spec$data, spec$mapping) +
            layer(geom = geom, stat = spec$stat, position = spec$position, params = spec$params) +
            scale_pattern_identity() + spec$extra
        pattern_names(grid::forceGrob(layer_grob(p)[[1L]]))
    }
    our_geoms <- grep("^Geom.+Pattern$", getNamespaceExports("ggpattern"), value = TRUE)
    our_geoms <- set_names(our_geoms, sub("Pattern$", "", our_geoms))
    # (every geom whose areas we assume the rows of has a spec)
    expect_in(setdiff(c(names(known_area_by_geoms), names(our_geoms)), names(suggested)), names(specs))
    expect_in(names(suggested), c(names(known_area_by_geoms), names(our_geoms)))

    for (name in names(specs)) {
        spec <- specs[[name]]
        geom <- getExportedValue("ggplot2", name)
        traced <- area_patterns(make_GeomPattern(geom, area_by = "trace"), spec)
        # (the areas were traced)
        expect_setequal(traced, spec$patterns)
        # the `area_by` we know for the geom
        if (name %in% names(known_area_by_geoms)) {
            expect_equal(known_area_by(geom), known_area_by_geoms[[name]], info = name)
            expect_equal(area_patterns(make_GeomPattern(geom), spec), traced, info = name)
        }
        # our own pattern geom for the geom
        if (name %in% names(our_geoms)) {
            ours <- getExportedValue("ggpattern", our_geoms[[name]])
            # (tracing fills each square of the multipolygon)
            if (name == "GeomSf") traced <- rle(traced)$values
            expect_equal(area_patterns(ours, spec), traced, info = name)
        }
    }
})

test_that("`make_GeomPattern()` traces the row of each area by its fill", {
    skip_if(getRversion() < "4.2.0")
    # a geom drawing its rectangles in reverse row order followed by a rectangle not from a row
    GeomRev <- ggproto("GeomRev", GeomRect,
        draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
            data <- data[rev(seq_len(nrow(data))), ]
            grob <- ggproto_parent(GeomRect, self)$draw_panel(data, panel_params, coord,
                                                              lineend = lineend, linejoin = linejoin)
            grid::grobTree(grob, grid::rectGrob(width = 0.1, height = 0.1, gp = grid::gpar(fill = "grey")))
        }
    )
    expect_equal(known_area_by(GeomRev), "trace")
    GeomRevPattern <- make_GeomPattern(GeomRev)
    df <- data.frame(x = 1:4, k = c("a", "b", "c", "d"), f = c("red", "green", NA, "blue"))
    p <- ggplot(df, aes(xmin = x, xmax = x + 0.9, ymin = 0, ymax = x, pattern = k, fill = I(f))) +
        layer(geom = GeomRevPattern, stat = "identity", position = "identity", params = list(alpha = 0.5)) +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch", "weave"))
    grob <- layer_grob(p)[[1]]
    # each area has the pattern of its row (the other rectangle is left alone)
    expect_equal(pattern_names(grob), c("weave", "crosshatch", "circle", "stripe"))
    # and its actual fill (with the alpha the geom gave it)
    expect_colours(area_fills(grob), c(alpha(c("blue", "green", "red"), 0.5), "grey"))
    expect_false(any(substr(all_fills(grob), 1L, 7L) %in% tracer_fill(1:4)))

    # rows that only differ by position have the same tracer fill
    # so a geom that needs constant aesthetics (e.g. a ribbon) is still drawn as one area
    GeomRibbonTraced <- make_GeomPattern(GeomRibbon, "GeomRibbonTraced", area_by = "trace")
    p <- ggplot(data.frame(x = 1:4), aes(x, ymin = 0, ymax = x)) +
        layer(geom = GeomRibbonTraced, stat = "identity", position = "identity",
              params = list(pattern = "circle", fill = "red"))
    grob <- layer_grob(p)[[1]]
    expect_equal(pattern_names(grob), "circle")
    expect_colours(area_fills(grob), "red")

    # a fill that can't be traced (e.g. a list of gradients): an area per row is assumed
    data <- data.frame(pattern = c("stripe", "circle"))
    data$fill <- list(grid::linearGradient(), "red")
    draw <- function(data) grid::rectGrob(x = c(0.25, 0.75), width = 0.4, gp = grid::gpar(fill = c("red", "blue")))
    expect_equal(pattern_names(draw_traced(draw, data)), c("stripe", "circle"))
})

test_that("areas that only exist when drawn are filled then", {
    skip_if(getRversion() < "4.2.0")
    # the dots of a dotplot are only made when its grob is drawn: always delayed
    GeomDotplotPattern <- make_GeomPattern(GeomDotplot)
    df <- data.frame(x = c(1, 1, 2, 3))
    p <- ggplot(df, aes(x)) +
        layer(geom = GeomDotplotPattern, stat = "bindot", position = "identity",
              params = list(binwidth = 0.5, pattern = "circle"))
    grob <- layer_grob(p)[[1]]
    expect_length(find_pattern_grobs(grob), 0L)
    withr::local_pdf(NULL)
    expect_no_error(print(p))
    expect_equal(pattern_names(grid::forceGrob(grob)), rep("circle", nrow(df)))

    # `with_pattern_aes()` needs `delay = TRUE` for any other such grob
    registerS3method("makeContent", "ggpattern_test_lazy",
                     function(x) grid::setChildren(x, grid::gList(grid::rectGrob())),
                     envir = asNamespace("grid"))
    lazy <- grid::gTree(cl = "ggpattern_test_lazy")
    data <- data.frame(pattern = "circle")
    expect_length(find_pattern_grobs(grid::forceGrob(with_pattern_aes(lazy, data))), 0L)
    expect_equal(pattern_names(grid::forceGrob(with_pattern_aes(lazy, data, delay = TRUE))), "circle")
    # which `make_GeomPattern()` uses for a grob with no areas when the panel is drawn
    GeomLazy <- ggproto("GeomLazy", Geom, required_aes = "x",
                        draw_panel = function(data, panel_params, coord) lazy,
                        draw_key = draw_key_rect)
    GeomLazyPattern <- make_GeomPattern(GeomLazy, area_by = function(data) data[1L, ])
    p <- ggplot(df, aes(x)) +
        layer(geom = GeomLazyPattern, stat = "identity", position = "identity", params = list(pattern = "circle"))
    expect_equal(pattern_names(grid::forceGrob(layer_grob(p)[[1]])), "circle")
    # (a wrong number of rows is only an error when the grob is drawn)
    GeomLazyPattern2 <- make_GeomPattern(GeomLazy, area_by = function(data) data[c(1L, 1L), ])
    p <- ggplot(df, aes(x)) +
        layer(geom = GeomLazyPattern2, stat = "identity", position = "identity", params = list(pattern = "circle"))
    grob <- layer_grob(p)[[1]]
    expect_error(grid::forceGrob(grob), "drew 1 area but the `area_by` function gave 2 rows",
                 class = "ggpattern_area_mismatch")

    # tracing is tried again when the grob is drawn if there were no traced areas when the panel was drawn
    registerS3method("makeContent", "ggpattern_test_lazy_fill",
                     function(x) {
                         n <- length(x$fill)
                         grid::setChildren(x, grid::gList(grid::rectGrob(
                             x = rev(seq_len(n)) / (n + 1), width = 0.1, gp = grid::gpar(fill = rev(x$fill))
                         )))
                     },
                     envir = asNamespace("grid"))
    GeomLazyFill <- ggproto("GeomLazyFill", Geom, required_aes = "x", default_aes = aes(fill = "grey"),
                            draw_panel = function(data, panel_params, coord) {
                                grid::gTree(fill = data$fill, cl = "ggpattern_test_lazy_fill")
                            },
                            draw_key = draw_key_rect)
    GeomLazyFillPattern <- make_GeomPattern(GeomLazyFill)
    df <- data.frame(x = 1:3, k = c("a", "b", "c"), f = c("red", "green", "blue"))
    p <- ggplot(df, aes(x, pattern = k, fill = I(f))) +
        layer(geom = GeomLazyFillPattern, stat = "identity", position = "identity") +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    grob <- layer_grob(p)[[1]]
    expect_s3_class(grob, "ggpattern_traced")
    forced <- grid::forceGrob(grob)
    expect_equal(pattern_names(forced), c("crosshatch", "circle", "stripe"))
    expect_colours(area_fills(forced), c("blue", "green", "red"))
    expect_no_error(print(p))
    # if there still aren't any it is drawn with its actual fills
    GeomNoArea <- ggproto("GeomNoArea", Geom, required_aes = "x", default_aes = aes(fill = "grey"),
                          draw_panel = function(data, panel_params, coord) {
                              grid::pointsGrob(data$x / 4, data$x / 4, pch = 21, gp = grid::gpar(fill = data$fill))
                          },
                          draw_key = draw_key_point)
    p <- ggplot(df, aes(x, fill = I(f))) +
        layer(geom = make_GeomPattern(ggproto(NULL, GeomNoArea, draw_panel = function(data, panel_params, coord) {
            grid::gTree(children = grid::gList(grid::linesGrob(gp = grid::gpar(fill = data$fill[1L]))))
        })), stat = "identity", position = "identity")
    forced <- grid::forceGrob(layer_grob(p)[[1]])
    expect_length(find_pattern_grobs(forced), 0L)
    expect_equal(all_fills(forced), "red")
})

test_that("`with_pattern()` makes a pattern layer from a layer", {
    skip_if(getRversion() < "4.2.0")
    with_pattern <- gridpattern::with_pattern
    df <- data.frame(x = c("a", "b", "c"), y = 3:1)
    # pattern aesthetics set to a constant
    layer <- with_pattern(geom_col(fill = "red"), pattern = "circle", pattern_fill = "blue")
    expect_s3_class(layer, "Layer")
    expect_s3_class(layer$geom, c("GeomColPattern", "GeomCol"))
    grob <- layer_grob(ggplot(df, aes(x, y)) + layer)[[1]]
    expect_equal(pattern_names(grob), rep("circle", 3L))
    data <- layer_data(ggplot(df, aes(x, y)) + layer)
    expect_equal(unique(data$pattern_fill), "blue")
    expect_equal(unique(data$fill), "red")

    # pattern aesthetic mappings (added to those of the layer)
    layer <- with_pattern(geom_col(aes(fill = x)), mapping = aes(pattern = x))
    p <- ggplot(df, aes(x, y)) + layer +
        scale_pattern_manual(values = c("stripe", "circle", "crosshatch"))
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle", "crosshatch"))
    expect_setequal(names(layer$mapping), c("fill", "pattern"))
    # the original layer isn't changed
    original <- geom_col()
    with_pattern(original, pattern = "circle")
    expect_s3_class(original$geom, "GeomCol")
    expect_null(original$aes_params$pattern)

    # our pattern geom is used if we have one for the layer's geom
    expect_identical(with_pattern(geom_rect())$geom, GeomRectPattern)
    expect_identical(with_pattern(geom_ribbon())$geom, GeomRibbonPattern)
    pd <- data.frame(x = c(0, 1, 1, 2, 3, 3), y = c(0, 0, 1, 0, 0, 1), g = rep(c("b", "a"), each = 3))
    p <- ggplot(pd, aes(x, y, group = g)) + with_pattern(geom_polygon(), mapping = aes(pattern = g)) +
        scale_pattern_manual(values = c("stripe", "circle"))
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("stripe", "circle"))
    # (but not for e.g. a layer with its own key glyph)
    layer <- with_pattern(geom_rect(key_glyph = "point"), pattern = "circle")
    expect_false(identical(layer$geom, GeomRectPattern))
    expect_s3_class(layer$geom, c("GeomRectPattern", "GeomRect"))

    # a dotplot's dots are filled when drawn
    layer <- with_pattern(geom_dotplot(binwidth = 0.5), pattern = "circle")
    p <- ggplot(data.frame(x = c(1, 1, 2, 3)), aes(x)) + layer
    withr::local_pdf(NULL)
    expect_no_error(print(p))
    expect_equal(pattern_names(grid::forceGrob(layer_grob(p)[[1]])), rep("circle", 4L))

    expect_error(with_pattern(geom_col(), fill = "red"), "must be named pattern aesthetics")

    # any other `{ggplot2}` object is returned as it is (whatever the other arguments)
    coord <- coord_polar()
    expect_identical(with_pattern(coord, pattern = "circle", fill = "red"), coord)
    others <- list(scale_fill_grey(), facet_wrap(vars(x)), theme_bw(), labs(x = "x"), guides(fill = "none"))
    for (other in others) expect_identical(with_pattern(other, pattern = "circle"), other)
    patterned <- with_pattern(c(list(geom_col()), others), pattern = "circle")
    expect_identical(patterned[[1L]]$geom, GeomColPattern)
    expect_identical(patterned[-1L], others)
    # (as is a `NULL` in a list e.g. from an `if ()` without an `else`)
    patterned <- with_pattern(list(geom_col(), if (FALSE) coord_flip(), others[[1L]]), pattern = "circle")
    expect_identical(patterned[-1L], list(NULL, others[[1L]]))
    expect_no_error(ggplot_build(ggplot(df, aes(x, y)) + patterned))
    # but not a plot
    expect_error(with_pattern(ggplot(df, aes(x, y)) + geom_col(), pattern = "circle"),
                 "can't make a pattern plot from a plot")
    # so a list of a layer and a coord (e.g. what `geom_sf()` returns) can be given
    skip_if_not_installed("sf")
    layers <- geom_sf(fill = "red")
    patterned <- with_pattern(layers, pattern = "circle", mapping = aes(pattern_fill = k))
    expect_length(patterned, length(layers))
    expect_identical(patterned[[1L]]$geom, GeomSfPattern)
    expect_equal(patterned[[1L]]$aes_params$pattern, "circle")
    expect_identical(patterned[[2L]], layers[[2L]])
    square <- function(x) sf::st_polygon(list(cbind(x + c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
    df <- sf::st_sf(k = c("a", "b"), geometry = sf::st_sfc(square(0), square(2)))
    p <- ggplot(df) + patterned
    expect_equal(pattern_names(layer_grob(p)[[1]]), c("circle", "circle"))
    expect_no_error(print(p))
})
