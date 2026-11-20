#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Default colour scales
#'
#' These are the default scales for the `pattern_colour`, `pattern_fill`, and `pattern_fill2` aesthetics.
#' See [ggplot2::scale_colour_continuous()] and [ggplot2::scale_colour_discrete()] for more information.
#'
#' The palette is the first of the `palette` argument,
#' the scale `type` (or arguments of [scale_pattern_colour_gradient()] or [scale_pattern_colour_hue()]),
#' the `palette.<aesthetic>.<type>` theme element (e.g. `palette.pattern_fill.discrete`, see [pattern_palettes]),
#' and the default palette ([scales::pal_seq_gradient()] from `"#132B43"` to `"#56B1F7"` for continuous scales
#' and [scales::pal_hue()] for discrete scales).
#' These scales don't use the `palette.colour.<type>` or `palette.fill.<type>` theme elements.
#'
#' @param ... Additional parameters passed on to the scale
#' @param palette A palette function (or something that [scales::as_continuous_pal()]
#'   or [scales::as_discrete_pal()] can convert into one e.g. a character vector of colours).
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param guide A function used to create a guide or its name.
#' @param na.value Colour to use for missing values.
#' @param type One of "gradient" or "viridis" indicating the
#'   colour scale to use.
#'   If `NULL` (the default unless the `ggplot2.continuous.colour` or `ggplot2.continuous.fill` option is set)
#'   uses the theme's palette else the default palette.
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
#'
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_discrete(palette = scales::pal_brewer(palette = "Set1"))
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_continuous <- function(...,
                                            palette = NULL,
                                            aesthetics = "pattern_colour",
                                            guide = guide_colourbar(available_aes = standardise_aes_names(aesthetics)),
                                            na.value = "grey50",
                                            type = getOption("ggplot2.continuous.colour")) {
  pattern_colour_continuous(..., palette = palette, aesthetics = aesthetics,
                            guide = guide, na.value = na.value, type = type,
                            gradient = scale_pattern_colour_gradient,
                            viridis = scale_pattern_colour_viridis_c)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_continuous <- function(...,
                                          palette = NULL,
                                          aesthetics = "pattern_fill",
                                          guide = guide_colourbar(available_aes = standardise_aes_names(aesthetics)),
                                          na.value = "grey50",
                                          type = getOption("ggplot2.continuous.fill")) {
  pattern_colour_continuous(..., palette = palette, aesthetics = aesthetics,
                            guide = guide, na.value = na.value, type = type,
                            gradient = scale_pattern_fill_gradient,
                            viridis = scale_pattern_fill_viridis_c)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_continuous <- function(...,
                                           palette = NULL,
                                           aesthetics = "pattern_fill2",
                                           guide = guide_colourbar(available_aes = standardise_aes_names(aesthetics)),
                                           na.value = "grey50",
                                           type = getOption("ggplot2.continuous.fill")) {
  pattern_colour_continuous(..., palette = palette, aesthetics = aesthetics,
                            guide = guide, na.value = na.value, type = type,
                            gradient = scale_pattern_fill2_gradient,
                            viridis = scale_pattern_fill2_viridis_c)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_discrete <- function(..., palette = NULL, aesthetics = "pattern_colour",
                                          na.value = "grey50") {
  pattern_colour_discrete(..., palette = palette, aesthetics = aesthetics,
                          na.value = na.value, hue = scale_pattern_colour_hue)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_discrete <- function(..., palette = NULL, aesthetics = "pattern_fill",
                                        na.value = "grey50") {
  pattern_colour_discrete(..., palette = palette, aesthetics = aesthetics,
                          na.value = na.value, hue = scale_pattern_fill_hue)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_colour_continuous
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_discrete <- function(..., palette = NULL, aesthetics = "pattern_fill2",
                                         na.value = "grey50") {
  pattern_colour_discrete(..., palette = palette, aesthetics = aesthetics,
                          na.value = na.value, hue = scale_pattern_fill2_hue)
}

pattern_colour_continuous <- function(..., palette, aesthetics, guide, na.value, type,
                                      gradient, viridis) {
  has_old_args <- any(...names() %in% c("low", "high", "space"))
  if (is.null(palette) && (has_old_args || !is.null(type))) {
    scale_fn <- switch(type %||% "gradient",
                       gradient = gradient,
                       viridis = viridis,
                       abort("Unknown scale type"))
    return(scale_fn(..., aesthetics = aesthetics, guide = guide, na.value = na.value))
  }
  continuous_scale(aesthetics,
                   palette = as_scale_palette(palette, type = "continuous",
                                              aesthetic = standardise_aes_names(aesthetics)[1]),
                   fallback.palette = pal_seq_gradient("#132B43", "#56B1F7"),
                   guide = guide,
                   na.value = na.value,
                   ...)
}

pattern_colour_discrete <- function(..., palette, aesthetics, na.value, hue) {
  has_old_args <- any(...names() %in% c("h", "c", "l", "h.start", "direction"))
  if (is.null(palette) && has_old_args) {
    return(hue(..., aesthetics = aesthetics, na.value = na.value))
  }
  discrete_scale(aesthetics,
                 palette = as_scale_palette(palette, type = "discrete",
                                            aesthetic = standardise_aes_names(aesthetics)[1]),
                 fallback.palette = pal_hue(),
                 na.value = na.value,
                 ...)
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
