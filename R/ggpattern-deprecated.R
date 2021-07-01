#' Deprecated data/functions
#'
#' These data/functions are Deprecated in this release of ggpattern,
#' they will be marked as Defunct and removed in a future version.
#'
#' \enumerate{
#' \item{For `magick_filter_names` use `magick::filter_types()` instead.}
#' \item{For `magick_gravity_names` use `magick::gravity_types()` instead.}
#  \item{For `magick_pattern_intensity_names` use `gridpattern::names_magick_intensity`.}
#  \item{For `magick_pattern_names` use `gridpattern::names_magick`.}
#  \item{For `magick_pattern_stripe_names` use `gridpattern::names_magick_stripe`.}
#  \item{For `placeholder_names` use `gridpattern::names_placeholder`.}
#' }
#' @param polygon_df polygon df
#' @param width Width
#' @param height Height
#' @param img `magick` image
#' @param type `magick` type
#' @param gravity `magick` gravity
#' @param filter `magick` filter
#' @param colour1 Colour
#' @param colour2 Colour
#' @param orientation Orientation
#' @param scale Scale
#' @inheritParams ggplot2::flip_data
#' @inheritParams ggplot2::gg_dep
#' @inheritParams ggplot2::has_flipped_aes
#' @inheritParams ggplot2::remove_missing
#' @inheritParams ggplot2::should_stop
#' @inheritParams scales::alpha
#' @examples
#'     suppressWarnings({
#'       print(magick_filter_names)
#'       print(magick_gravity_names)
#'       print(magick_pattern_intensity_names)
#'       print(magick_pattern_names)
#'       print(magick_pattern_stripe_names)
#'       print(placeholder_names)
#'     })
#' @name ggpattern-deprecated
#' @aliases magick_filter_names magick_gravity_names magick_pattern_intensity_names magick_pattern_names magick_pattern_stripe_names placeholder_names
NULL

#' @export magick_filter_names
NULL

#' @export magick_gravity_names
NULL

#' @export magick_pattern_intensity_names
NULL

#' @export magick_pattern_names
NULL

#' @export magick_pattern_stripe_names
NULL

#' @export placeholder_names
NULL

#' @rdname ggpattern-deprecated
#' @export
alpha <- function(colour, alpha = NA) {
    .Deprecated("scales::alpha()")
    scales::alpha(colour, alpha)
}

#' @rdname ggpattern-deprecated
#' @export
has_flipped_aes <- function(data, params = list(), main_is_orthogonal = NA,
                            range_is_orthogonal = NA, group_has_equal = FALSE,
                            ambiguous = FALSE, main_is_continuous = FALSE) {
  # Is orientation already encoded in data?
  if (!is.null(data$flipped_aes)) {
    not_na <- which(!is.na(data$flipped_aes))
    if (length(not_na) != 0) {
      return(data$flipped_aes[[not_na[1L]]])
    }
  }

  # Is orientation requested in the params
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }

  x <- data$x %||% params$x
  y <- data$y %||% params$y
  xmin <- data$xmin %||% params$xmin
  ymin <- data$ymin %||% params$ymin
  xmax <- data$xmax %||% params$xmax
  ymax <- data$ymax %||% params$ymax

  # Does a single x or y aesthetic corespond to a specific orientation
  if (!is.na(main_is_orthogonal) && xor(is.null(x), is.null(y))) {
    return(is.null(y) == main_is_orthogonal)
  }

  has_x <- !is.null(x)
  has_y <- !is.null(y)

  # Does a provided range indicate an orientation
  if (!is.na(range_is_orthogonal)) {
    if (!is.null(ymin) || !is.null(ymax)) {
      return(!range_is_orthogonal)
    }
    if (!is.null(xmin) || !is.null(xmax)) {
      return(range_is_orthogonal)
    }
  }

  # If ambiguous orientation = NA will give FALSE
  if (ambiguous && (is.null(params$orientation) || is.na(params$orientation))) {
    return(FALSE)
  }

  # Is there a single actual discrete position
  y_is_int <- is.integer(y)
  x_is_int <- is.integer(x)
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }

  # Does each group have a single x or y value
  if (group_has_equal) {
    if (has_x) {
      if (length(x) == 1) return(FALSE)
      x_groups <- vapply(split(data$x, data$group), function(x) length(unique(x)), integer(1))
      if (all(x_groups == 1)) {
        return(FALSE)
      }
    }
    if (has_y) {
      if (length(y) == 1) return(TRUE)
      y_groups <- vapply(split(data$y, data$group), function(x) length(unique(x)), integer(1))
      if (all(y_groups == 1)) {
        return(TRUE)
      }
    }
  }

  # give up early
  if (!has_x && !has_y) {
    return(FALSE)
  }

  # Both true discrete. give up
  if (y_is_int && x_is_int) {
    return(FALSE)
  }
  # Is there a single discrete-like position
  y_is_int <- if (has_y) isTRUE(all.equal(y, round(y))) else FALSE
  x_is_int <- if (has_x) isTRUE(all.equal(x, round(x))) else FALSE
  if (xor(y_is_int, x_is_int)) {
    return(y_is_int != main_is_continuous)
  }
  # Is one of the axes a single value
  if (all(x == 1)) {
    return(main_is_continuous)
  }
  if (all(y == 1)) {
    return(!main_is_continuous)
  }
  # If both are discrete like, which have most 0 or 1-spaced values
  y_diff <- diff(sort(y))
  x_diff <- diff(sort(x))

  if (y_is_int && x_is_int) {
    return((sum(x_diff <= 1) < sum(y_diff <= 1)) != main_is_continuous)
  }

  y_diff <- y_diff[y_diff != 0]
  x_diff <- x_diff[x_diff != 0]

  # If none are discrete is either regularly spaced
  y_is_regular <- if (has_y && length(y_diff) != 0) all((y_diff / min(y_diff)) %% 1 < .Machine$double.eps) else FALSE
  x_is_regular <- if (has_x && length(x_diff) != 0) all((x_diff / min(x_diff)) %% 1 < .Machine$double.eps) else FALSE
  if (xor(y_is_regular, x_is_regular)) {
    return(y_is_regular != main_is_continuous)
  }
  # default to no
  FALSE
}

#' @rdname ggpattern-deprecated
#' @export
gg_dep <- function(version, msg) {
  .Deprecated("ggplot2::gg_dep()")
  v <- as.package_version(version)
  cv <- utils::packageVersion("ggplot2")
  text <- "{msg} (Defunct; last used in version {version})"

  # If current major number is greater than last-good major number, or if
  #  current minor number is more than 1 greater than last-good minor number,
  #  give error.
  if (cv[[1,1]] > v[[1,1]]  ||  cv[[1,2]] > v[[1,2]] + 1) {
    abort(glue(text))

  # If minor number differs by one, give warning
  } else if (cv[[1,2]] > v[[1,2]]) {
    warn(glue(text))

  # If only subminor number is greater, give message
  } else if (cv[[1,3]] > v[[1,3]]) {
    message(glue(text))
  }

  invisible()
}

#' @rdname ggpattern-deprecated
#' @export
remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  if (!is.logical(na.rm)) {
    abort("`na.rm` must be logical")
  }

  missing <- detect_missing(df, vars, finite)

  if (any(missing)) {
    df <- df[!missing, ]
    if (!na.rm) {
      if (name != "") name <- paste(" (", name, ")", sep = "")
      str <- if (finite) "non-finite" else "missing"
      warning_wrap(
        "Removed ", sum(missing), " rows containing ", str, " values", name, "."
      )
    }
  }
  df
}
detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}
is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}
# Wrapper around is.finite to handle list cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    is.finite(x)
  }
}

#' @rdname ggpattern-deprecated
#' @export
should_stop <- function(expr) {
    .Deprecated("ggplot2::should_stop()")
    ggplot2::should_stop(expr)
}

#' @rdname ggpattern-deprecated
#' @export
flip_data <- function(data, flip = NULL) {
  flip <- flip %||% any(data$flipped_aes) %||% FALSE
  if (isTRUE(flip)) {
    names(data) <- switch_orientation(names(data))
  }
  data
}
switch_orientation <- function(aesthetics) {
  # We should have these as globals somewhere
  x <- ggplot_global$x_aes
  y <- ggplot_global$y_aes
  x_aes <- match(aesthetics, x)
  x_aes_pos <- which(!is.na(x_aes))
  y_aes <- match(aesthetics, y)
  y_aes_pos <- which(!is.na(y_aes))
  if (length(x_aes_pos) > 0) {
    aesthetics[x_aes_pos] <- y[x_aes[x_aes_pos]]
  }
  if (length(y_aes_pos) > 0) {
    aesthetics[y_aes_pos] <- x[y_aes[y_aes_pos]]
  }
  aesthetics
}

#' @rdname ggpattern-deprecated
#' @export
flipped_names <- function(flip = FALSE) {
  .Deprecated("ggplot2::flipped_names()")
  x_aes <- ggplot_global$x_aes
  y_aes <- ggplot_global$y_aes
  if (flip) {
    ret <- as.list(c(y_aes, x_aes))
  } else {
    ret <- as.list(c(x_aes, y_aes))
  }
  names(ret) <- c(x_aes, y_aes)
  ret
}

#' @rdname ggpattern-deprecated
#' @export
convert_polygon_df_to_alpha_channel <- function(polygon_df, width, height) {
  .Deprecated(msg = 'convert_polygon_df_to_alpha_channel() is deprecated')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the polygon to an actual grob, coloured 'black'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gp <- gpar(fill = 'black')
  boundary_grob <- convert_polygon_df_to_polygon_grob(polygon_df, gp=gp)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the grob as an image of the given size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  png_file <- tempfile(fileext = ".png")
  png(png_file, width=width, height=height)
  grid.draw(boundary_grob)
  dev.off()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load the file and convert o a numeric matrix with values 0/1 depending
  # on whether the pixel is white or black.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  alpha_channel <- png::readPNG(png_file)
  alpha_channel <- alpha_channel[,,1] < 0.5
  storage.mode(alpha_channel) <- 'numeric'

  # t(alpha_channel)
  alpha_channel
}

#' @rdname ggpattern-deprecated
#' @export
convert_img_to_array <- function(img) {
  .Deprecated(msg = 'convert_img_to_array() is deprecated')

  stopifnot(inherits(img, 'magick-image'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # extract the RGB array from that image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- as.numeric(magick::image_data(img))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is a grey image (i.e. a 2d matrix), then promote it
  # to a 3d array by copying the grey into R,G and B planes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(dim(arr)) == 2) {
    arr <- array(c(arr, arr, arr), dim = c(dim(arr), 3))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add an alpha channel if there isn't one already
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (dim(arr)[3] == 3) {
    alpha_matrix <- matrix(1, nrow=dim(arr)[1], ncol = dim(arr)[2])
    arr          <- my_abind(arr, alpha_matrix)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check: Assert everything image is RGBA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(dim(arr)[3] == 4)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transpose the image if requested.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (transpose) {
  #   arr <- aperm(arr, c(2, 1, 3))
  # }

  arr
}

#' @rdname ggpattern-deprecated
#' @export
create_gradient_img <- function(width       = 100,
                                height      = 100,
                                colour1     = 'red',
                                colour2     = 'blue',
                                orientation = 'vertical') {
  .Deprecated(msg = 'create_gradient_img() is deprecated')

  colour1     <- convert_r_colour_to_magick_colour(colour1)
  colour2     <- convert_r_colour_to_magick_colour(colour2)
  colour_spec <- paste0(colour2, "-", colour1)

  if (orientation == 'radial') {
    colour_spec <- paste0(colour1, "-", colour2)
    pseudo <- paste0('radial-gradient:', colour_spec)
    img <- magick::image_blank(width, height, pseudo_image = pseudo)
  } else if (orientation == 'vertical') {
    pseudo <- paste0('gradient:', colour_spec)
    img <- magick::image_blank(width, height, pseudo_image = pseudo)
  } else if (orientation == 'horizontal') {
    pseudo <- paste0('gradient:', colour_spec)
    img <- magick::image_blank(height, width, pseudo_image = pseudo)
    img <- magick::image_rotate(img, 90)
  } else {
    abort("create_gradient_img() - Orientation not supported: ", orientation)
  }

  img
}
convert_r_colour_to_magick_colour <- function(col) {
  if (is.null(col) || is.na(col) || length(col) == 0) {
    return('none')
  }
  rgb(t(col2rgb(col)), maxColorValue = 255)
}

#' @rdname ggpattern-deprecated
#' @export
fetch_placeholder_img <- function(width = 100, height = 100, type = 'kitten') {

  .Deprecated(msg = 'fetch_placeholder_img() is deprecated')
  width  <- as.integer(width)
  height <- as.integer(height)

  img_url <- switch(
    type,
    kitten         = glue::glue("https://placekitten.com/{width}/{height}"),
    kittenbw       = glue::glue("https://placekitten.com/g/{width}/{height}"),
    picsum         = glue::glue("https://picsum.photos/{width}/{height}"),
    picsumbw       = glue::glue("https://picsum.photos/{width}/{height}?grayscale"),
    murray         = glue::glue("http://www.fillmurray.com/{width}/{height}"),
    murraybw       = glue::glue("http://www.fillmurray.com/g/{width}/{height}"),
    cage           = glue::glue("https://www.placecage.com/{width}/{height}"),
    cagebw         = glue::glue("https://www.placecage.com/g/{width}/{height}"),
    bear           = glue::glue("https://placebear.com/{width}/{height}"),
    bearbw         = glue::glue("https://placebear.com/g/{width}/{height}"),
    seagal         = glue::glue("https://www.stevensegallery.com/{width}/{height}"),
    seagalbw       = glue::glue("https://www.stevensegallery.com/g/{width}/{height}"),
    placeholderbw  = ,
    placeholder    = glue::glue("https://via.placeholder.com/{width}x{height}.png"),
    dummybw        = ,
    dummy          = glue::glue("https://dummyimage.com/{width}x{height}"),
    flickr         = glue::glue("https://loremflickr.com/{width}/{height}"),
    flickrbw       = glue::glue("https://loremflickr.com/g/{width}/{height}/all"),
    beard          = glue::glue("http://placebeard.it/{width}/{height}"),
    beardbw        = glue::glue("http://placebeard.it/g/{width}/{height}"),
    lorempixel     = glue::glue("http://lorempixel.com/{width}/{height}"),
    lorempixelbw   = glue::glue("http://lorempixel.com/g/{width}/{height}"),
    placeimg       = glue::glue("http://placeimg.com/{width}/{height}/any"),
    placeimgbw     = glue::glue("http://placeimg.com/{width}/{height}/any/grayscale"),
    keanu          = glue::glue("https://placekeanu.com/{width}/{height}"),
    keanubw        = glue::glue("https://placekeanu.com/{width}/{height}/g"),
    {
      # warn("fetch_placeholder_image_as_array(): Unknown pattern_type (", type, ") ",
      #         "using 'kitten'")
      glue::glue("https://placekitten.com/{width}/{height}")
    }
  )

  img_read_memoised(filename = img_url)
}
img_read <- function(filename) {
  if (identical(filename, '')) {
    return(magick::image_blank(100, 100, color = 'none'))
  }
  if (is.null(filename) || length(filename)==0 || is.na(filename) || filename == '') {
    abort("img_read(): bad filename: ", deparse(filename), call.=FALSE)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch the URL as an image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- tryCatch(
    {magick::image_read(filename)},
    error = function(cond) {
      message(cond)
      stop("img_read() non-specific error with magick::image_read(", shQuote(filename), ")", call. = FALSE)
    }
  )
  img
}
# `img_read_memoised()` defined in zzz.R

fill_types <- c('fit', 'expand', 'squish', 'none', 'tile')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize image, preserving aspect, such that one of target width or height is achieved without distoring or cropping.
#'
#' The canvas of the image will be expanded to cover the full width/height but
#' will be transparent.
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' \dontrun{
#' filename <- system.file("img", "Rlogo.png", package="png")
#' img <- magick::image_read(filename)
#' fill_area_with_img_fit(img, 100, 400)
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_area_with_img_fit <- function(img, width, height, gravity = 'Center', filter = 'lanczos') {

  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = TRUE)
  img      <- magick::image_resize(img, geometry = geometry, filter=filter)
  img      <- magick::image_extent(img, geometry, gravity = gravity)

  img
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize image, ignoring aspect, such that both the target width and height is achieved.
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' \dontrun{
#' filename <- system.file("img", "Rlogo.png", package="png")
#' img <- magick::image_read(filename)
#' fill_area_with_img_squish(img, 100, 400)
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_area_with_img_squish <- function(img, width, height, filter='lanczos') {
  geometry <- magick::geometry_size_pixels(width = width, height = height,
                                           preserve_aspect = FALSE)
  img <- magick::image_resize(img, geometry = geometry, filter=filter)
  img
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize image, preserving aspect, such that when cropped, the target width and height is achieved.
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' \dontrun{
#' filename <- system.file("img", "Rlogo.png", package="png")
#' img <- magick::image_read(filename)
#' fill_area_with_img_expand(img, 100, 400)
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_area_with_img_expand <- function(img, width, height, gravity = 'Center', filter='lanczos') {
  img_info   <- magick::image_info(img)
  img_aspect <- img_info$width/img_info$height
  area_aspect <- width/height
  if (area_aspect > img_aspect) {
    scale_width  <- width
    scale_height <- width / img_aspect
  } else if (area_aspect < img_aspect) {
    scale_height <- height
    scale_width  <- height * img_aspect
  } else {
    scale_height <- height
    scale_width  <- width
  }

  # expand
  geometry <- magick::geometry_size_pixels(width = scale_width, height = scale_height, preserve_aspect = FALSE)
  img      <- magick::image_resize(img, geometry = geometry, filter=filter)

  # crop
  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = FALSE)
  img      <- magick::image_crop(img, geometry = geometry, gravity = gravity)

  img
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Resize image canvas to the target width/height, and use gravity to place the undistorted image
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' \dontrun{
#' filename <- system.file("img", "Rlogo.png", package="png")
#' img <- magick::image_read(filename)
#' fill_area_with_img_none(img, 100, 400)
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_area_with_img_none <- function(img, width, height, gravity = 'Center',
                                    filter = 'lanczos', scale = 1) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale if requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (scale == -1) {
    info   <- magick::image_info(img)
    scale  <-  width/info$width
  } else if (scale == -2) {
    info   <- magick::image_info(img)
    scale  <-  height/info$height
  }

  if (scale != 1) {
    geometry <- magick::geometry_size_percent(width = scale * 100)
    img      <- magick::image_resize(img, geometry, filter = filter)
  }


  geometry <- magick::geometry_size_pixels(width = width, height = height, preserve_aspect = TRUE)
  img      <- magick::image_extent(img, geometry, gravity = gravity)

  img
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Tile image to fill the specified area
#'
#' Unless the width and height are carefully chosen, this operation will distort
#' the image to force it to fit the dimensions
#'
#' @inheritParams fill_area_with_img
#'
#' @return magick image of the required dimensions
#'
#' @examples
#' \dontrun{
#' filename <- system.file("img", "Rlogo.png", package="png")
#' img <- magick::image_read(filename)
#' fill_area_with_img_none(img, 100, 400)
#' }
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fill_area_with_img_tile <- function(img, width, height, filter = filter, scale = 1) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale if requested
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (scale == -1) {
    info <- magick::image_info(img)
    scale  <-  width/info$width
  } else if (scale == -2) {
    info <- magick::image_info(img)
    scale  <-  height/info$height
  }

  if (scale != 1) {
    geometry <- magick::geometry_size_percent(width = scale * 100)
    img      <- magick::image_resize(img, geometry, filter = filter)
  }

  img <- magick::image_flip(img)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save the source tile locally
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tile_temp_filename <- tempfile(fileext = ".png")
  magick::image_write(img, path = tile_temp_filename)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tile the image using command line 'imagemagick'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tmp_filename <- tempfile(fileext = ".png")
  command <- glue::glue("convert -size {width}x{height} tile:'{tile_temp_filename}' ",
                        "-background none {tmp_filename}")

  system(command)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read in the tiled image and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- magick::image_read(tmp_filename)
  img <- magick::image_flip(img)

  img
}




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fill an area with a magick image
##
## Fill an area with a magick image
##
## Support fill types
##
## \itemize{
## \item{\code{fit   }}{ - Fill image canvas, preserving aspect, such that one of target width or height is achieved without distoring or cropping.}
## \item{\code{squish}}{ - Fill image canvas, ignoring aspect, such that both the target width and height is achieved.}
## \item{\code{expand}}{ - Fill image canvas, by scaling image and preserving aspect, such that when cropped, the target width and height is achieved.}
## \item{\code{none  }}{ - Fill image canvas canvas to the target width/height, and use gravity to place the undistorted image}
## \item{\code{tile  }}{ - Tile the image canvas with image.}
## }
##
##
##
## @param img magick image
## @param width,height target dimensions for final image
## @param type how to resize the image. 'none', 'fit', 'squish', 'expand' or 'tile'. default: squish
## @param gravity how to anchor the image during the operation. See \code{magick::gravity_types()}
## @param filter magick filter type. default: lanczos. See \code{magick::filter_types()}
##        for more information.  This option has no effect for \code{type = 'none'}.
## @param scale scale the source image before tiling. default: 1
## @param filter default: lanczos
##
## @return magick image of the required dimensions
##
## @import magick
##
## @examples
## \dontrun{
## filename <- system.file("img", "Rlogo.png", package="png")
## img <- magick::image_read(filename)
## fill_area_with_img(img, 100, 400, type = 'squish')
## }
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
#' @rdname ggpattern-deprecated
fill_area_with_img <- function(img, width, height, type='squish',
                               gravity = 'Center', filter='lanczos',
                               scale = 1) {

  .Deprecated(msg = 'fill_area_with_img() is deprecated')
  if (length(width) != 1 || length(height) != 1 ||
      is.na(width) || is.na(height) ||
      width <= 0 || height <= 0) {
    return(img)
  }

  type <- check_default(type, fill_types)

  switch(
    type,
    fit    = fill_area_with_img_fit   (img, width, height, gravity = gravity, filter = filter),
    expand = fill_area_with_img_expand(img, width, height, gravity = gravity, filter = filter),
    squish = fill_area_with_img_squish(img, width, height                   , filter = filter),
    none   = fill_area_with_img_none  (img, width, height, gravity = gravity, filter = filter, scale = scale),
    tile   = fill_area_with_img_tile  (img, width, height,                    filter = filter, scale = scale),
    {
      warning("fill_area_with_img(): resize not understood: '", type,
              "', using 'squish'")
      fill_area_with_img_squish(img, width, height)
    }
  )
}

check_default <- function(x, options = NULL, default = NULL, type = NULL, prefix = "", verbose = FALSE) {

  stopifnot(is.null(options) || is.atomic(options))

  default <- default %||% (options[1])
  if (is.null(default) || length(default) != 1) {
    stop("check_default(): Must specify 'default' or 'options'")
  }


  if (length(x) != 1) {
    if (verbose) {
      warning("check_default(): ", prefix,
              " Value should be length 1, but got ", deparse(x),
              ". Using default: ", default, call.=FALSE)
    }
    res <- default
  } else if (!is.null(options) && !x %in% options) {
    if (verbose) {
      warning("check_default(): ", prefix,
              " Value should be one of ", deparse(options),
              " but got ", deparse(x),
              ". Using default: ", default, call.=FALSE)
    }
    res <- default
  } else {
    res <- x
  }

  if (!is.null(type)) {
    res <- switch(
      type,
      numeric   =,
      number    =,
      float     =,
      num       = ifelse(is.numeric  (res), res, default),
      character = ,
      chr       = ,
      char      = ifelse(is.character(res), res, default),
      {
        stop("check_default(): Don't know how to check for type: ", type)
      }
    )
  }

  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' abind clone for adding a matrix to an array
#'
#' A very cut-down version of \code{abind::abind()} that will only stack a
#' matrix onto an array.
#'
#' @param arr,mat array and matrix to be stacked
#'
#' @return new array with matrix added as a new plane at the end of the array
#'
#' @importFrom utils head
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_abind <- function(arr, mat) {

  stopifnot(is.array(arr))
  stopifnot(is.matrix(mat))
  if (!identical(head(dim(arr), -1), dim(mat))) {
    stop("Dimension missmatch. Array: ", deparse(dim(arr)), "  Matrix: ", deparse(dim(mat)))
  }

  new_dim    <- dim(arr)
  new_dim[3] <- new_dim[3] + 1

  array(c(arr, mat), dim = new_dim)
}
