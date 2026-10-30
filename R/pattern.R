gridpattern_pattern <- function(params, boundary_df, legend = FALSE) {

    args <- munge_params(params)
    args$prefix <- ""
    args$legend <- legend
    args$x <- boundary_df$x
    args$y <- boundary_df$y
    args$id <- boundary_df$id
    do.call(gridpattern::patternGrob, args)
}

# unlist to support e.g. multi-valued pattern args
munge_params <- function(params) {
    l <- as.list(params)
    nms <- grep("^pattern_", names(l), value = TRUE)
    # must avoid unlisting `grid::pattern()` fill values
    nms <- grep("^pattern_fill", nms, invert = TRUE, value = TRUE)
    for (nm in nms) {
        l[[nm]] <- unlist(l[[nm]])
    }
    l
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
#'                   and/or grid grobs to use as an alpha mask
#'
#' @return grobTree containing all the pattern grobs
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_grobs <- function(all_params, boundaries) {

  # There might be a number of polygons coming in.
  # There should be one row in the 'all_params' data.frame for each polygon
  # There should be the matching 'id' for the polygon in the 'boundaries'
  pattern_grobs <- lapply(seq(nrow(all_params)), function(i) {
    params      <- all_params[i,]
    boundary <- boundaries[[i]]

    if (is.grob(boundary)) {
        boundary_df <- convert_grob_to_polygon_df(boundary)
    } else if (is_polygon_df(boundary)) {
        boundary_df <- boundary
    } else {
        abort("boundary must either be a grob or 'polygon_df'")
    }

    grob <- gridpattern_pattern(params, boundary_df, legend = FALSE)
    if (is.grob(boundary)) {
        if (inherits(grob, "alpha_mask")) {
            grob <- editGrob(grob, masker = boundary)
        } else {
            grob <- gridpattern::alphaMaskGrob(grob, boundary,
                                               res = params$pattern_res)
        }
    }
    grob
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Wrap the list of pattern grob objects into a grobTree
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pattern_grobs <- do.call(grid::grobTree, pattern_grobs)
}
