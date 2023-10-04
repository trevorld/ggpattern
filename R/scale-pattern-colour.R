#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Continuous and binned colour scales
#'
#' See \code{ggplot2::scale_colour_continuous()} for more information
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of "gradient" (the default) or "viridis" indicating the
#'   colour scale to use
#' @examples
#'   if (require("ggplot2")) {
#'     df <- data.frame(level = c("a", "b", "c", "d"),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = outcome),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_continuous()
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
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
