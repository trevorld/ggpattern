get_aspect_ratio <- function() {
    width <- as.numeric(convertWidth(unit(1, "npc"), "in"))
    height <- as.numeric(convertHeight(unit(1, "npc"), "in"))
    width / height
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill each area of a grob with the pattern of its row of `params`
#'
#' A wrapper around [gridpattern::with_pattern()] taking our pattern aesthetics.
#'
#' @param grob A grid grob (usually the grob drawn by the parent `{ggplot2}` geom).
#' @param params A data frame of (pattern) aesthetics with either
#'               one row per area of `grob` (in the order of [gridpattern::area_boundaries()])
#'               (or per unique value of `group`) or a single row used for every area.
#' @param group,legend Passed to [gridpattern::with_pattern()].
#' @return A grob.
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
with_pattern_rows <- function(grob, params, group = NULL, legend = FALSE) {
    # (`gridpattern::with_pattern()` checks there is one row per area (or group) or a single row)
    args <- c(list(grob, pattern = params$pattern %||% "stripe"),
              as.list(params[grep("^pattern_", names(params))]),
              list(group = group, legend = legend, prefix = ""))
    do.call(gridpattern::with_pattern, args)
}

# The first row of each group of `data` (in the order {ggplot2} draws its groups' polygons)
first_rows_by_group <- function(data) {
    data <- data[!duplicated(data$group), , drop = FALSE]
    data[order(data$group), , drop = FALSE]
}
