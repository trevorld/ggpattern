# the grobs of `class` within `x` (in drawing order, not looking within them)
find_grobs <- function(x, class) {
    if (inherits(x, class)) return(list(x))
    if (inherits(x, "gTree")) x <- x$children[x$childrenOrder]
    if (inherits(x, "gList")) unlist(lapply(x, find_grobs, class = class), recursive = FALSE)
}

# the pattern grobs within `x` (in drawing order)
find_pattern_grobs <- function(x) find_grobs(x, "pattern")

# the names of the patterns of the pattern grobs within `x` (in drawing order)
pattern_names <- function(x) {
    vapply(find_pattern_grobs(x), function(g) g$pattern, character(1L), USE.NAMES = FALSE)
}

# the classes of the (non-gTree) grobs within `x`
grob_classes <- function(x) {
    if (inherits(x, "gTree")) unlist(lapply(x$children, grob_classes)) else class(x)[1L]
}

# Registers (until the end of the calling test) a pattern that records the params it is called with.
# Returns a function that prints a plot and returns the recorded params.
local_capture_pattern <- function(name, defaults = list(), .local_envir = parent.frame()) {
    captured <- new.env()
    capture_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
        captured$params <- params
        grid::nullGrob()
    }
    gridpattern::local_pattern(name, capture_pattern, defaults = defaults,
                               env = globalenv(), .local_envir = .local_envir)
    withr::local_pdf(NULL, .local_envir = .local_envir)
    function(p) {
        print(p)
        captured$params
    }
}

# the fills of the (non-gTree) grobs within `x` (in drawing order)
all_fills <- function(x) {
    if (inherits(x, "pattern")) return(NULL)
    if (inherits(x, "gTree")) {
        return(unname(unlist(lapply(x$children[x$childrenOrder], all_fills))))
    }
    unname(x$gp$fill)
}

# the fills of the areas within `x` (in drawing order)
area_fills <- function(x) {
    fills <- unname(all_fills(x))
    fills[!is.na(fills) & fills != "transparent"]
}

# expects the colours `object` to be the same colours as `expected` (however they are written)
expect_colours <- function(object, expected) {
    expect_equal(grDevices::col2rgb(object, alpha = TRUE), grDevices::col2rgb(expected, alpha = TRUE))
}

# the names of our exported pattern geoms named by their `{ggplot2}` counterpart
# e.g. `c(GeomRect = "GeomRectPattern")`
our_pattern_geoms <- function() {
    geoms <- sort(grep("^Geom.+Pattern$", getNamespaceExports("ggpattern"), value = TRUE))
    stats::setNames(geoms, sub("Pattern$", "", geoms))
}

# a unit square `sf` polygon whose lower left corner is at `(x, 0)`
sf_square <- function(x) sf::st_polygon(list(cbind(x + c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))
