

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
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_polygon_df <- function(x) {
  is.null(x) ||
    (is.data.frame(x) && all(c('x', 'y', 'id') %in% names(x)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate the bounding box of a polygon_df object
#'
#' @param polygon_df polygon_df data.frame
#'
#' @return 4-element numeric vector of \code{c(xmin, ymin, xmax, ymax)}
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calculate_bbox_polygon_df <- function(polygon_df) {

  stopifnot(is_polygon_df(polygon_df))

  x <- range(polygon_df$x)
  y <- range(polygon_df$y)
  c(x[1], y[1], x[2], y[2])
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a \code{polygon_df} to \code{grid::polygonGrob} object
#'
#' @param polygon_df polygon_df data.frame
#' @param default.units 'npc
#' @param gp default: gpar()
#'
#' @return sf polygon object
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_polygon_df_to_polygon_grob <- function(polygon_df, default.units = 'npc',
                                                gp = gpar()) {


  if (is.null(polygon_df) || nrow(polygon_df) < 3) {
    return(grid::nullGrob())
  }

  grid::polygonGrob(
    x             = polygon_df$x,
    y             = polygon_df$y,
    id            = polygon_df$id,
    default.units = default.units,
    gp            = gp
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a \code{polygon_df} to an \code{sf} POLYGON/MULTIPOLYGON
#'
#' @param polygon_df Polygon data.frame
#' @param buffer_dist buffer the polygon by the given distance
#'
#' @return sf polygon object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_polygon_df_to_polygon_sf <- function(polygon_df, buffer_dist = 0) {

  if (!requireNamespace("sf"))
      abort(c("Suggested package {sf} must be installed",
              i = 'Install using `install.packages("sf")`'))
  if (is.null(polygon_df) || nrow(polygon_df) < 3 ||
      anyNA(polygon_df$x) || anyNA(polygon_df$y)) {
    return(sf::st_polygon())
  }

  polys <- split(polygon_df, polygon_df$id)

  create_coords <- function(poly) {
    xs <- poly$x
    ys <- poly$y

    # {sf} wants explicitly closed polygons, so set the last point
    # to be the same as the first
    if (xs[1] != tail(xs, 1) || ys[1] != tail(ys, 1)) {
      xs <- c(xs, xs[1])
      ys <- c(ys, ys[1])
    }

    list(cbind(xs, ys))
  }

  all_coords <- lapply(polys, create_coords)

  res <- sf::st_multipolygon(all_coords)

  # perform a zero-buffer operation to remove self-intersection
  # As suggested here:
  #  - https://gis.stackexchange.com/questions/163445/getting-topologyexception-input-geom-1-is-invalid-which-is-due-to-self-intersec#163480
  # https://gis.stackexchange.com/questions/223252/how-to-overcome-invalid-input-geom-and-self-intersection-when-intersecting-shape
  res <- sf::st_buffer(res, buffer_dist)
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a sf POLYGON/MULTIPOLYGON into a polygon_df
#'
#' @param mp sf POLYGON or MULTIPOLYGON object
#'
#' @return polygon_df data.frame object
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_polygon_sf_to_polygon_df <- function(mp) {
  if (!requireNamespace("sf"))
      abort(c("Suggested package {sf} must be installed",
              i = 'Install using `install.packages("sf")`'))
  mat <- as.matrix(mp)

  if (inherits(mp, 'POLYGON')) {
    poly_lengths <- nrow(mat)
  } else if (inherits(mp, 'MULTIPOLYGON')) {
    poly_lengths <- vapply(mp, function(x) {nrow(x[[1]])}, integer(1))
  } else if (sf::st_is_empty(mp)) {
    return(mat)
  } else {
    warn("convert_polygon_sf_to_polygon_df(): Not POLYGON or MULTIPOLYGON: ", deparse(class(mp)))
    return(NULL)
  }
  id  <- rep.int(seq_along(poly_lengths), times = poly_lengths)

  create_polygon_df(x=mat[,1], y=mat[,2], id=id)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simple 2D rotation of a polygon about the origin
#'
#' @param polygon_df polygon data.frame
#' @param angle angle in degrees
#' @param aspect_ratio aspect ratio
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rotate_polygon_df <- function(polygon_df, angle, aspect_ratio) {

  angle <- angle * pi/180

  new_x <- polygon_df$x * cos(angle) - polygon_df$y * sin(angle)
  new_y <- polygon_df$x * sin(angle) + polygon_df$y * cos(angle)

  polygon_df$x <- new_x
  polygon_df$y <- new_y

  polygon_df
}

if (FALSE) {
  polygon_df <- create_polygon_df(x=c(0, 0.3, 0.3, 0,  0.5, 0.9, 0.9, 0.5),
                                    y=c(0, 0, 0.3, 0.3,  0.5, 0.5, 0.9, 0.9),
                                    id = c(1, 1, 1, 1, 2, 2, 2, 2))

  sfob <- NULL
  sfob <- convert_polygon_df_to_polygon_sf(polygon_df)
  plot(sfob)

  convert_polygon_sf_to_polygon_df(sfob)


  grob <- convert_polygon_df_to_polygon_grob(polygon_df, gp = gpar(fill = 'black'))
  plot.new()
  grid.draw(grob)
}
