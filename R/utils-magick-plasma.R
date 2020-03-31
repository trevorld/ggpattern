
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create plasma using imagemagick
#'
#' Ref: \url{https://www.imagemagick.org/Usage/canvas/}
#'
#' @param width,height image dimensions
#' @param colour colour
#'
#' @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_plasma_img <- function(width=100, height=100, colour) {

  colour <- convert_r_colour_to_magick_colour(colour)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a pattern image of the required size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pseudo <- paste0("plasma:")
  img    <- magick::image_blank(width, height, pseudo_image = pseudo)
  img    <- magick::image_convert(img, colorspace = 'gray', depth = 8)
  img    <- magick::image_blur(img, radius = 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make the white transparent
  # Colourie the black pixels into the desired colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- magick::image_transparent(img, 'white')
  img <- magick::image_colorize(img, opacity = 50, colour)

  img
}
