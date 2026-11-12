# the grobs of `class` within `x` (in drawing order, not looking within them)
find_grobs <- function(x, class) {
    if (inherits(x, class)) return(list(x))
    if (inherits(x, "gTree")) {
        unlist(lapply(x$children[x$childrenOrder], find_grobs, class = class), recursive = FALSE)
    }
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
