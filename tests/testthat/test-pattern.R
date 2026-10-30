test_that("pattern-specific defaults registered with {gridpattern} apply", {

    captured <- new.env()
    capture_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
        captured$params <- params
        grid::nullGrob()
    }
    name <- "ggpattern_test_capture"
    gridpattern::register_pattern(
        name,
        capture_pattern,
        defaults = list(
            pattern_type = "running",
            pattern_subtype = "lefthand",
            pattern_gravity = "north",
            pattern_filter = "box",
            pattern_shape = "star5",
            pattern_scale = 2,
            pattern_frequency = 0.5,
            pattern_fill2 = function(params, gp) params$pattern_fill,
            pattern_phase = 0.25,
            pattern_option_1 = 7,
            pattern_angle = 45,
            pattern_density = 0.5,
            pattern_colour = "blue",
            pattern_size = 2,
            pattern_grid = "hex",
            # a parameter {ggpattern} doesn't know about
            pattern_mortar = function(params, gp) 2 * params$pattern_spacing
        ),
        env = globalenv()
    )
    on.exit(gridpattern::unregister_pattern(name, env = globalenv()), add = TRUE)

    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    p <- ggplot(df, aes(trt, outcome)) +
        geom_col_pattern(pattern = name, pattern_fill = "red", pattern_spacing = 0.1)

    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    print(p)

    params <- captured$params
    # our `NaN` defaults are treated as "unset" so the pattern-specific defaults apply
    expect_equal(params$pattern_type, "running")
    expect_equal(params$pattern_subtype, "lefthand")
    expect_equal(params$pattern_gravity, "north")
    expect_equal(params$pattern_filter, "box")
    expect_equal(params$pattern_shape, "star5")
    expect_equal(params$pattern_scale, 2)
    expect_equal(params$pattern_frequency, 0.5)
    expect_equal(params$pattern_phase, 0.25)
    expect_equal(params$pattern_option_1, 7)
    expect_equal(params$pattern_option_2, 0) # {gridpattern}'s generic default
    expect_equal(params$pattern_fill2, "red")
    expect_equal(params$pattern_mortar, 0.2)
    expect_equal(params$pattern_angle, 45)
    expect_equal(params$pattern_density, 0.5)
    expect_equal(params$pattern_colour, "blue")
    expect_equal(params$pattern_size, 2)
    expect_equal(params$pattern_grid, "hex")
    # or else {gridpattern}'s generic defaults
    expect_equal(params$pattern_linetype, 1)
    expect_equal(params$pattern_rot, 0)
    expect_equal(params$pattern_orientation, "vertical")
    expect_equal(params$pattern_filename, "")
    expect_equal(params$pattern_xoffset, 0)
    expect_equal(params$pattern_units, "snpc")
    # but explicitly set aesthetics take precedence
    expect_equal(params$pattern_spacing, 0.1)

    # an explicitly set `pattern_fill2` is passed through
    p2 <- ggplot(df, aes(trt, outcome)) +
        geom_col_pattern(pattern = name, pattern_fill = "red", pattern_fill2 = "blue")
    print(p2)
    expect_equal(captured$params$pattern_fill2, "blue")
    # as is an `NA` (i.e. transparent) one
    p2 <- ggplot(df, aes(trt, outcome)) +
        geom_col_pattern(pattern = name, pattern_fill = "red", pattern_fill2 = NA)
    print(p2)
    expect_true(is.na(captured$params$pattern_fill2) && !is.nan(captured$params$pattern_fill2))

    # an explicitly set (or scale-mapped) `NA` `pattern_shape` is passed through
    # (it draws nothing for "pch") unlike our `NaN` default
    p3 <- ggplot(df, aes(trt, outcome)) +
        geom_col_pattern(pattern = name, pattern_shape = NA)
    print(p3)
    expect_true(is.na(captured$params$pattern_shape) && !is.nan(captured$params$pattern_shape))
    p4 <- ggplot(df, aes(trt, outcome, pattern_shape = trt)) +
        geom_col_pattern(pattern = name) +
        scale_pattern_shape_manual(values = c(a = 1, b = NA))
    print(p4)
    expect_true(is.na(captured$params$pattern_shape) && !is.nan(captured$params$pattern_shape))
})

test_that("builtin `pattern_fill2` defaults are unchanged", {

    captured <- new.env()
    capture_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
        captured$params <- params
        grid::nullGrob()
    }
    name <- "ggpattern_test_capture"
    # same defaults as the builtin "crosshatch" / "weave" patterns
    gridpattern::register_pattern(
        name,
        capture_pattern,
        defaults = list(pattern_fill2 = function(params, gp) params$pattern_fill),
        env = globalenv()
    )
    on.exit(gridpattern::unregister_pattern(name, env = globalenv()), add = TRUE)

    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)

    print(ggplot(df, aes(trt, outcome)) + geom_col_pattern(pattern = name, pattern_fill = "red"))
    expect_equal(captured$params$pattern_fill2, "red")

    # and the generic default when the pattern doesn't declare one
    gridpattern::register_pattern(name, capture_pattern, env = globalenv())
    print(ggplot(df, aes(trt, outcome)) + geom_col_pattern(pattern = name, pattern_fill = "red"))
    expect_equal(captured$params$pattern_fill2, "#4169E1")
})

test_that("unset aesthetics use {gridpattern}'s defaults", {

    captured <- new.env()
    capture_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
        captured$params <- params
        grid::nullGrob()
    }
    name <- "ggpattern_test_capture"
    gridpattern::register_pattern(name, capture_pattern, env = globalenv())
    on.exit(gridpattern::unregister_pattern(name, env = globalenv()), add = TRUE)

    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    print(ggplot(df, aes(trt, outcome)) + geom_col_pattern(pattern = name))
    params <- captured$params
    # same values as our previous fixed defaults
    expect_equal(params$pattern_angle, 30)
    expect_equal(params$pattern_density, 0.2)
    expect_equal(params$pattern_colour, "grey20")
    expect_equal(params$pattern_fill, "grey80")
    expect_equal(params$pattern_fill2, "#4169E1")
    expect_equal(params$pattern_linewidth, 0.5)
    expect_equal(params$pattern_alpha, NA_real_)
    expect_equal(params$pattern_grid, "square")

    # and the builtin patterns' own defaults apply e.g. the "text" pattern's shape and fontsize
    print(ggplot(df, aes(trt, outcome)) + geom_col_pattern(pattern = "text"))
    grid::grid.force()
    # text grobs drawn within {gridpattern} pattern grobs (i.e. not the axis labels)
    find_text <- function(x, in_pattern = FALSE) {
        in_pattern <- in_pattern || inherits(x, "pattern")
        if (in_pattern && inherits(x, "text")) return(list(x))
        if (inherits(x, "gTree")) {
            unlist(lapply(x$children, find_text, in_pattern = in_pattern), recursive = FALSE)
        }
    }
    text_grobs <- find_text(grid::grid.grab())
    expect_gt(length(text_grobs), 0L)
    labels <- unique(unlist(lapply(text_grobs, function(x) x$label)))
    expect_equal(labels, "X")
    fontsizes <- unique(unlist(lapply(text_grobs, function(x) x$gp$fontsize)))
    expect_equal(fontsizes, 12)
})
