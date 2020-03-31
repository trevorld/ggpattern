
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Names of patterns available in image magick, plus subsets for shaded intensity and stripes
#'
#' See \url{http://www.imagemagick.org/script/formats.php} for more information.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_names <- c(
  "bricks", "checkerboard", "circles", "crosshatch", "crosshatch30",
  "crosshatch45", "fishscales", "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100", "hexagons", "horizontal", "horizontal2",
  "horizontal3", "horizontalsaw", "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "leftshingle", "octagons", "right30", "right45", "rightshingle",
  "smallfishscales", "vertical", "vertical2", "vertical3", "verticalbricks",
  "verticalleftshingle", "verticalrightshingle", "verticalsaw"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname magick_pattern_names
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_intensity_names <- c(
  "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname magick_pattern_names
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_stripe_names <- c(
  "crosshatch", "crosshatch30", "crosshatch45",
  "horizontal", "horizontal2", "horizontal3",
  "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "right30", "right45",
  "vertical", "vertical2", "vertical3"
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create one of imagemagick's internal patterns
#'
#' These are all 2-colour pixel patterns - the 'white' part will be made transparent.
#'
#' @param width,height image dimensions
#' @param type name of the imagemagick pattern. See \url{http://www.imagemagick.org/script/formats.php}
#'        for more information. See \link{magick_pattern_names} for a list of all
#'        supported imagemagick patterns.
#' @param colour colour used to draw the pattern
#'
#' @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_pattern_img <- function(width=100, height=100, type = 'hexagons',
                                      colour = 'black') {

  type   <- check_default(tolower(type), magick_pattern_names, default = 'checkerboard')
  colour <- convert_r_colour_to_magick_colour(colour)

  if (width == 0 || height == 0) {
    warn("create_magick_pattern_img(): zero size")
    return(magick::image_blank(10, 10))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a pattern image of the required size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pseudo <- paste0("pattern:", type)
  img    <- magick::image_blank(width, height, pseudo_image = pseudo)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The checkerboard is the only(?) pattern which isn't pure black and white.
  # for the sake of consistency it will be thresholded from its original
  # two-level gray colours into pure black and white.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (type %in% 'checkerboard') {
    img <- magick::image_threshold(img, type = 'black')
    img <- magick::image_threshold(img, type = 'white')
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make the white transparent
  # Colourie the black pixels into the desired colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- magick::image_transparent(img, 'white')
  img <- magick::image_colorize(img, opacity = 100, colour)

  img
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a scaled version one of imagemagick's internal patterns
#'
#' These are all 2-colour pixel patterns - the 'white' part will be made transparent.
#'
#' @inheritParams create_magick_pattern_img
#' @param scale pattern scaling factor defualt: 1
#' @param filter filter to apply when sacling pattern.  default: 'box' for crisp
#'        edges to the pixel.  See \code{magick::filter_types()} for a full list
#'        of filters.
#'
#' @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_pattern_img_scaled <- function(width  = 100,
                                             height = 100,
                                             type   = 'hexagons',
                                             colour = 'black',
                                             scale  = 1,
                                             filter = 'box') {

  scale <- check_default(scale, default = 1, type = 'numeric')

  if (scale < 0.01 || scale > 100) {
    scale <- 1
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a scaled version of the pattern
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_width  <- width /scale
  new_height <- height/scale

  img <- create_magick_pattern_img(new_width, new_height, type = type, colour = colour)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Which we'll scale back down to the original size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- fill_area_with_img_squish(img, width, height, filter = filter)

  img
}

