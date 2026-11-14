#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a `polygon_df` object (deprecated)
#'
#' `create_polygon_df()` and `is_polygon_df()` are deprecated since nothing exported accepts a `polygon_df`.
#' A `polygon_df` is just a data frame with `x`, `y`, and `id` columns
#' (e.g. the `boundary_df` that `{gridpattern}` passes to a custom geometry pattern function).
#' To fill the areas of a grob with patterns see [with_pattern_aes()].
#'
#' `create_polygon_df()` creates a `polygon_df` object from the given coordinates.
#' Code using `polygon_df` should not assume that the first and last point
#' within each id are the same i.e. it may have to manually set a final
#' point equal to the initial point if that is what its graphics system desires.
#'
#' @param x,y For `create_polygon_df()` the coordinates of the polygon (not necessarily closed).
#'   For `is_polygon_df()` `x` is an object.
#' @param id a numeric vector used to separate locations in x,y into multiple polygons
#'
#' @return `create_polygon_df()` returns a data frame with `x`, `y`, and `id` columns.
#'   `is_polygon_df()` returns `TRUE` if `x` is a `polygon_df` or `NULL`.
#' @keywords internal
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_polygon_df <- function(x, y, id = 1L) {
  deprecate_soft("1.4.0", "create_polygon_df()",
                 details = "Use a data frame with `x`, `y`, and `id` columns instead.")
  data_frame(
    x     = x,
    y     = y,
    id    = id
  )
}

#' @rdname create_polygon_df
#' @export
is_polygon_df <- function(x) {
  deprecate_soft("1.4.0", "is_polygon_df()")
  is.null(x) ||
    (is.data.frame(x) && all(c('x', 'y', 'id') %in% names(x)))
}
