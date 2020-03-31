

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate an RGBA array and create a raster grob to fill the region
#'
#' the boundary of the shaded region will be used to generate a alpha mask
#' so that the array conforms to the shape.
#'
#' @inheritParams create_pattern_none
#' @param type type of image to fill with.  'image' for a file,
#'             placeholder for an iamge from a web placeholder site,
#'             gradient for an image magick gradient
#'
#' @return rasterGrob
#'
#' @import grid
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_array <- function(params, boundary_df, aspect_ratio, legend, type = 'image') {

  if (anyNA(boundary_df$x) || anyNA(boundary_df$y)) {
    return(grid::nullGrob())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # I don't have a good answer on how to make a legend.  Images are fetched
  # for the actual plot based upon the size of the feature in the plot, but
  # the legend has no idea on this size so won't be able to fetch the same
  # image for display in the legend.  Leaving blank for now.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (legend && type %in% c('placeholder')) {
  #   return(grid::nullGrob())
  # }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What is the size of the bounding box of the boundary for this pattern?
  # Calculate the centre (x,y) and (width,height)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  npc_xrange  <- range(boundary_df$x, na.rm = TRUE)
  npc_yrange  <- range(boundary_df$y, na.rm = TRUE)
  npc_width   <- abs(diff(npc_xrange))
  npc_height  <- abs(diff(npc_yrange))
  npc_x       <- mean(npc_xrange)
  npc_y       <- mean(npc_yrange)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the Native dimensions of the bounding box and use the integer
  # values of these to define the image dimensions.
  # Need to scale the y-dimension by the aspect_ratio
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  native_width  <- grid::convertWidth(unit(npc_width , 'npc'), 'native')
  native_height <- grid::convertWidth(unit(npc_height, 'npc'), 'native')
  arr_width     <- as.integer(native_width)
  arr_height    <- as.integer(as.numeric(native_height) / aspect_ratio)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is a legend, then draw a much smaller imagee.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (legend) {
    arr_width  <- as.integer( arr_width / 10)
    arr_height <- as.integer(arr_height / 10)

    # Override type for better looking legend when tiling
    if (params$pattern_type %in% c('tile', 'none')) {
      params$pattern_type <- 'fit'
    }

    if (params$pattern %in% c('magick')) {
      params$pattern_scale <- params$pattern_scale * params$pattern_key_scale_factor
    } else {
      params$pattern_scale <- params$pattern_key_scale_factor
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The image and the boundary_df bbox are coincident.  To mask the image,
  # scale the boundary_df to encompass the full 'npc' range from 0 to 1.
  # An alpha mask will be created to encompass the whole of the image, and
  # the resulting image will be placed at the bbox location.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scaled_boundary_df   <- boundary_df
  scaled_boundary_df$x <- (scaled_boundary_df$x - npc_xrange[1]) / npc_width
  scaled_boundary_df$y <- (scaled_boundary_df$y - npc_yrange[1]) / npc_height


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch an image of the required dimensions.
  # Create a mask of the required dimensions from the scaled boundary_df
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  user_array_funcs  <- getOption('ggpattern_array_funcs')

  if (type %in% names(user_array_funcs)) {
    arr_func <- user_array_funcs[[type]]
    rgba_arr  <- arr_func(width = arr_width, height = arr_height, params = params, legend = legend)
  } else if (type %in% names(array_pattern_funcs)) {
    arr_func <- array_pattern_funcs[[type]]
    rgba_arr  <- arr_func(width = arr_width, height = arr_height, params = params, legend = legend)
  } else {
    warn("create_pattern_array(): unknwon type '", type, "'", call. = FALSE)
    return(grid::nullGrob())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check the array conforms to what we want.  This is especially
  # important as we're allowing users to generate arrays for patterns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!(is.array(rgba_arr)             &&
        is.numeric(rgba_arr)           &&
        length(dim(rgba_arr) == 3)     &&
        dim(rgba_arr)[3] == 4          &&
        dim(rgba_arr)[1] == arr_height &&
        dim(rgba_arr)[2] == arr_width  &&
        !anyNA(rgba_arr)               &&
        min(rgba_arr) >= 0             &&
        max(rgba_arr) <= 1)) {

    warn(glue::glue("create_pattern_array(): Expecting a numeric RGBA array with dim = c({arr_height}, {arr_width}, 4) ",
                    "but instead got a {deparse(class(rgba_arr))} ",
                    "of type {typeof(rgba_arr)} with dimensions {deparse(dim(rgba_arr))}"), call. = FALSE)

    rgba_arr <- array(c(0, 1), dim = c(arr_height, arr_width, 4))
  }

  boundary_mask <- convert_polygon_df_to_alpha_channel(scaled_boundary_df, width = arr_width, height = arr_height)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Stack the current image array with an alpha channel.
  # Using a custom version of `abind::abind()` so I could avoid having it
  # as another package dependency
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgba_arr[,,4] <- rgba_arr[,,4] * boundary_mask * params$pattern_alpha


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a rasterGrob image at the location of the 'boundary_df' bounding box.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  raster_grob   <- rasterGrob(
    rgba_arr,
    x      = npc_x,
    y      = npc_y,
    width  = npc_width,
    height = npc_height
  )


  raster_grob
}
