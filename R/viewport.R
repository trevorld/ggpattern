


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the aspect ratio from the coord
#'
#' @param coord ggplot2 Coord object
#' @param panel_params panel parameters
#'
#' @return aspect ratio or NULL if none present in the coord
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_aspect_ratio_from_coord <- function(coord, panel_params) {
  if (!inherits(coord, "Coord") || is.null(coord$ratio)) {
    return(NULL)
  }
  1/coord$ratio * diff(panel_params$x.range) / diff(panel_params$y.range)
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get the aspec ratio of  the current viewport
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_aspect_ratio_from_current_viewport <- function() {
  vp <- grid::current.viewport()
  xrange <- abs(diff(vp$xscale))
  yrange <- abs(diff(vp$yscale))
  xrange/yrange
}



