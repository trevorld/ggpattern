

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# list of functions to handle the core 'grid' based patterns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
array_pattern_funcs <- list(
  image       = img_read_as_array_wrapper     ,
  placeholder = fetch_placeholder_array       ,
  gradient    = create_gradient_as_array      ,
  magick      = create_magick_pattern_as_array,
  plasma      = create_magick_plasma_as_array
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# list of functions to handle the core 'grid' based patterns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
grid_pattern_funcs <- list(
  none       = create_pattern_none,
  stripe     = create_pattern_stripes_via_sf,
  crosshatch = create_pattern_crosshatch_via_sf,
  circle     = create_pattern_circles
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Try and get an aspect ratio from the context of the plot
#'
#' @param coord current "Coord"
#' @param panel_params panel params
#'
#' @return Aspect ratio
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_aspect_ratio_from_context <- function(coord, panel_params) {
  if (!is.null(get_aspect_ratio_from_coord(coord, panel_params))) {
    aspect_ratio_from_context <- get_aspect_ratio_from_coord(coord, panel_params)
    # message("aspect_ratio_from_coord: ", round(aspect_ratio_from_context, 3))
  } else if (!is.null(get_aspect_ratio_from_current_viewport())) {
    aspect_ratio_from_context <- get_aspect_ratio_from_current_viewport()
    # message("aspect_ratio_from_viewport: ", round(aspect_ratio_from_context, 3))
  } else {
    message("create_pattern_grobs() - aspect ratio failure. using 1")
    aspect_ratio_from_context <- 1
  }

  aspect_ratio_from_context
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate all the pattern grobs for this geom
#'
#' @param all_params parameters for all the elements
#' @param boundary_dfs boundary_df objects for each of the elements
#' @param aspect_ratio aspect ratio
#'
#' @return grobTree containing all the pattern grobs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_grobs <- function(all_params, boundary_dfs, aspect_ratio) {

  # There might be a number of polygons coming in.
  # There should be one row in the 'all_params' data.frame for each polygon
  # There should be the matching 'id' for the polygon in the 'boundary_dfs'
  pattern_grobs <- lapply(seq(nrow(all_params)), function(i) {
    params      <- all_params[i,]
    boundary_df <- boundary_dfs[[i]]

    # print(params)
    # message("create_pattern_grobs(): ", params$pattern)
    # flush.console()

    if (is.null(params$pattern) || is.na(params$pattern) ||
        params$pattern %in% c('NA', '')) {
      return(NULL)
    }

    par <- params$pattern_aspect_ratio
    if (!is.null(par) && !is.na(par)) {
      aspect_ratio  <- par
    }
    # message("Final AspectRatio: ", aspect_ratio)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if the user has supplied a function to render this pattern
    # If so, get the function render the grobs and return, otherwise check
    # the grid patterns and the 'img' patterns to find the matching pattern
    # generating function
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pattern_name <- as.character(params$pattern)

    user_geometry_funcs <- getOption('ggpattern_geometry_funcs')
    user_array_funcs    <- getOption('ggpattern_array_funcs')

    if (pattern_name %in% names(user_geometry_funcs)) {
      pattern_func <- user_geometry_funcs[[pattern_name]]
      pattern_grob <- pattern_func(params = params, boundary_df = boundary_df, aspect_ratio = aspect_ratio)
    } else if (pattern_name %in% names(user_array_funcs)) {
      pattern_grob <- create_pattern_array(params = params, boundary_df = boundary_df, aspect_ratio = aspect_ratio, type = pattern_name, legend = FALSE)
    } else if (pattern_name %in% names(grid_pattern_funcs)) {
      pattern_func <- grid_pattern_funcs[[pattern_name]]
      pattern_grob <- pattern_func(params = params, boundary_df = boundary_df, aspect_ratio = aspect_ratio)
    } else if (pattern_name %in% names(array_pattern_funcs)) {
      pattern_grob <- create_pattern_array(params = params, boundary_df = boundary_df, aspect_ratio = aspect_ratio, type = pattern_name, legend = FALSE)
    } else {
      warn("create_pattern_grobs() - pattern not supported: ", deparse(pattern_name))
      pattern_grob <- grid::nullGrob()
    }

    pattern_grob

  })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Wrap the list of pattern grob objects into a grobTree
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pattern_grobs <- do.call(grid::grobTree, pattern_grobs)
}
