#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a \code{polygon_df} object from the given coordinates
#'
#' code using \code{polygon_df} should not assume that the first and last point
#' within each id are the same.  i.e. they may have to manulaly set a final
#' point equal to the initial point if that is what their graphics system
#' desires
#'
#' @param x,y coordinates of polygon. not necessarily closed.
#' @param id a numeric vector used to separate locations in x,y into multiple polygons
#'
#' @return data.frame with x, y, id columns.
#' @examples
#'   df <- create_polygon_df(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0))
#'   is_polygon_df(df)
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_polygon_df <- function(x, y, id = 1L) {
  data_frame(
    x     = x,
    y     = y,
    id    = id
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Test if object is polygon_df or NULL
#'
#' @param x object
#'
#' @return TRUE if object is polygon_df or NULL
#' @examples
#'   df <- create_polygon_df(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0))
#'   is_polygon_df(df)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_polygon_df <- function(x) {
  is.null(x) ||
    (is.data.frame(x) && all(c('x', 'y', 'id') %in% names(x)))
}
