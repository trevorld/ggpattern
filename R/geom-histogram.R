#' @rdname geom-docs
#' @export
geom_histogram_pattern <- make_constructor(
  GeomBarPattern, stat = "bin", position = "stack",
  # Passed to bin stat:
  binwidth = NULL, bins = NULL, orientation = NA
)
