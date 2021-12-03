#' Deprecated data/functions
#'
#' These data/functions are Deprecated in this release of ggpattern,
#' they will be marked as Defunct and removed in a future version.
#'
#' @name ggpattern-deprecated
NULL

#' @rdname ggpattern-deprecated
#' @param polygon_df A `polygon_df` object
#' @param default.units See [grid::polygonGrob()].
#' @param gp See [grid::polygonGrob()].
#' @export
convert_polygon_df_to_polygon_grob <- function(polygon_df, default.units = 'npc',
                                                gp = gpar()) {
  .Deprecated(msg = 'convert_polygon_df_to_polygon_grob() is deprecated')

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

#' @rdname ggpattern-deprecated
#' @param buffer_dist See [sf::st_buffer()].
#' @export
convert_polygon_df_to_polygon_sf <- function(polygon_df, buffer_dist = 0) {
  .Deprecated(msg = 'convert_polygon_df_to_polygon_sf() is deprecated')

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

#' @rdname ggpattern-deprecated
#' @param mp A `sf` package polygon object
#' @export
convert_polygon_sf_to_polygon_df <- function(mp) {
  .Deprecated(msg = 'convert_polygon_sf_to_polygon_df() is deprecated')
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

