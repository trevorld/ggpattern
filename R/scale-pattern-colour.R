
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Continuous and binned colour scales
#'
#' See \code{ggplot2::scale_colour_continuous()} for more information
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of "gradient" (the default) or "viridis" indicating the
#'   colour scale to use
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_continuous <- function(...,
                                    type = getOption("ggplot2.continuous.colour", default = "gradient")) {
  switch(
    type,
    gradient = scale_pattern_colour_gradient(...),
    viridis  = scale_pattern_colour_viridis_c(...),
    abort("Unknown scale type")
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_continuous <- function(...,
                                          type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  switch(
    type,
    gradient = scale_pattern_fill_gradient(...),
    viridis  = scale_pattern_fill_viridis_c(...),
    abort("Unknown scale type")
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_continuous <- function(...,
                                          type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  switch(
    type,
    gradient = scale_pattern_fill2_gradient(...),
    viridis  = scale_pattern_fill2_viridis_c(...),
    abort("Unknown scale type")
  )
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_continuous
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_colour_binned <- function(...,
#                                 type = getOption("ggplot2.continuous.colour", default = "gradient")) {
#   switch(
#     type,
#     gradient = scale_pattern_colour_steps(...),
#     viridis  = scale_pattern_colour_viridis_b(...),
#     abort("Unknown scale type")
#   )
# }

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_continuous
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_fill_binned <- function(...,
#                               type = getOption("ggplot2.continuous.colour", default = "gradient")) {
#   switch(
#     type,
#     gradient = scale_pattern_fill_steps(...),
#     viridis  = scale_pattern_fill_viridis_b(...),
#     abort("Unknown scale type")
#   )
# }
