gridpattern_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    args <- as.list(params)
    args$prefix <- ""
    args$legend <- legend
    args$x <- boundary_df$x
    args$y <- boundary_df$y
    args$id <- boundary_df$id
    do.call(gridpattern::patternGrob, args)
}

fill_default_params <- function(params) {
    pat <- params$pattern
    if (is.na(params$pattern_fill2)) {
        params$pattern_fill2 <- switch(pat,
                                       crosshatch = params$pattern_fill,
                                       weave = params$pattern_fill,
                                       '#4169E1')
    }
    if (pat == "regular_polygon" && is.numeric(params$pattern_shape))
        params$pattern_shape <- "convex6"
    params
}

get_aspect_ratio <- function() {
    width <- as.numeric(convertWidth(unit(1, "npc"), "in"))
    height <- as.numeric(convertHeight(unit(1, "npc"), "in"))
    aspect_ratio <-  width / height
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate all the pattern grobs for this geom
#'
#' @param all_params parameters for all the elements
#' @param boundaries boundary_df objects for each of the elements
#'                   and/or grid grobs to use as a clipping path
#' @param aspect_ratio aspect ratio
#'
#' @return grobTree containing all the pattern grobs
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_grobs <- function(all_params, boundaries, aspect_ratio) {

  # There might be a number of polygons coming in.
  # There should be one row in the 'all_params' data.frame for each polygon
  # There should be the matching 'id' for the polygon in the 'boundaries'
  pattern_grobs <- lapply(seq(nrow(all_params)), function(i) {
    params      <- fill_default_params(all_params[i,])
    boundary <- boundaries[[i]]

    par <- params$pattern_aspect_ratio
    if (!is.null(par) && !is.na(par)) {
      aspect_ratio  <- par
    }

    if (is.grob(boundary)) {
        boundary_df <- convert_grob_to_polygon_df(boundary)
    } else if (is_polygon_df(boundary)) {
        boundary_df <- boundary
    } else {
        abort("boundary must either be a grob or 'polygon_df'")
    }

    if (is.null(params$pattern_res) || is.na(params$pattern_res)) {
        width <- diff(range(boundary_df$x))
        native <- as.numeric(grid::convertWidth(unit(width, "npc"), "native"))
        inches <- as.numeric(grid::convertWidth(unit(width, "npc"), "in"))
        params$pattern_res <- 1.14 * native / inches
    }

    grob <- gridpattern_pattern(params, boundary_df, aspect_ratio, legend = FALSE)
    if (is.grob(boundary)) {
        if (inherits(grob, "clipping_path")) {
            grob <- editGrob(grob, clipper = boundary)
        } else {
            grob <- gridpattern::clippingPathGrob(grob, boundary)
        }
    }
    grob
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Wrap the list of pattern grob objects into a grobTree
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pattern_grobs <- do.call(grid::grobTree, pattern_grobs)
}
