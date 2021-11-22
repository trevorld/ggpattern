#' Defunct data/functions
#'
#' These data/functions are Defunct in this release of ggpattern.
#' \enumerate{
#' \item{For `magick_filter_names` use `magick::filter_types()` instead.}
#' \item{For `magick_gravity_names` use `magick::gravity_types()` instead.}
#  \item{For `magick_pattern_intensity_names` use `gridpattern::names_magick_intensity`.}
#  \item{For `magick_pattern_names` use `gridpattern::names_magick`.}
#  \item{For `magick_pattern_stripe_names` use `gridpattern::names_magick_stripe`.}
#  \item{For `placeholder_names` use `gridpattern::names_placeholder`.}
#' }
#' @param polygon_df polygon_df data.frame
#' @param angle angle in degrees
#' @param aspect_ratio aspect ratio
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
#' @aliases magick_filter_names magick_gravity_names magick_pattern_intensity_names magick_pattern_names magick_pattern_stripe_names placeholder_names
#' @name ggpattern-defunct
NULL

#' @rdname ggpattern-defunct
#' @export
calculate_bbox_polygon_df <- function(polygon_df) {
  .Defunct(msg = 'calculate_bbox_polygon_df() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
convert_polygon_df_to_alpha_channel <- function(polygon_df, width, height) {
  .Defunct(msg = 'convert_polygon_df_to_alpha_channel() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
rotate_polygon_df <- function(polygon_df, angle, aspect_ratio) {
  .Defunct(msg = 'rotate_polygon_df() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
convert_img_to_array <- function(img) {
  .Defunct(msg = 'convert_img_to_array() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
create_gradient_img <- function(width       = 100,
                                height      = 100,
                                colour1     = 'red',
                                colour2     = 'blue',
                                orientation = 'vertical') {
  .Defunct(msg = 'create_gradient_img() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
fetch_placeholder_img <- function(width = 100, height = 100, type = 'kitten') {
  .Defunct(msg = 'fetch_placeholder_img() is defunct')
}

#' @rdname ggpattern-defunct
#' @export
fill_area_with_img <- function(img, width, height, type='squish',
                               gravity = 'Center', filter='lanczos',
                               scale = 1) {
  .Defunct(msg = 'fill_area_with_img() is defunct')
}
