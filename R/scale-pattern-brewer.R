#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Sequential, diverging and qualitative colour scales from colorbrewer.org
#'
#' @description
#' The `brewer` scales provides sequential, diverging and qualitative
#' colour schemes from ColorBrewer. These are particularly well suited to
#' display discrete values on a map. See \url{https://colorbrewer2.org} for
#' more information.
#'
#' @note
#' The `distiller` scales extend brewer to continuous scales by smoothly
#' interpolating 7 colours from any palette to a continuous scale. The `fermenter`
#' scales provide binned versions of the brewer scales.
#'
#' @details
#' The `brewer` scales were carefully designed and tested on discrete data.
#' They were not designed to be extended to continuous data, but results often
#' look good. Your mileage may vary.
#'
#' @section Palettes:
#' The following palettes are available for use with these scales:
#' \describe{
#'   \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'   \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'   \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' Modify the palette through the `palette` arguement.
#'
#' @param direction,type,aesthetics,values,space,na.value,guide See \code{ggplot2::scale_colour_brewer} for more information.
#' @param palette If a string, will use that named palette. If a number, will index into
#'   the list of palettes of appropriate `type`. The list of available palettes can found
#'   in the Palettes section.
#' @param ... Other arguments passed on to [ggplot2::discrete_scale()], [ggplot2::continuous_scale()],
#'   or [ggplot2::binned_scale()], for `brewer`, `distiller`, and `fermenter` variants
#'   respectively, to control name, limits, breaks, labels and so forth.
#' @examples
#'   if (require("ggplot2")) {
#'     df <- data.frame(level = c("a", "b", "c", "d"),
#'                      outcome = c(2.3, 1.9, 3.2, 1))
#'     # discrete 'brewer' palette
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_brewer()
#'     plot(gg)
#'
#'     # continuous 'distiller' palette
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = outcome),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black'
#'       ) +
#'       theme_bw(18) +
#'       scale_pattern_fill_distiller()
#'     plot(gg)
#'   }
#' @return A [ggplot2::Scale] object.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "pattern_colour") {
  discrete_scale(aesthetics, 
                 palette = brewer_pal(type, palette, direction),
                 ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_brewer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "pattern_fill") {
  discrete_scale(aesthetics, 
                 palette = brewer_pal(type, palette, direction),
                 ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_brewer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_brewer <- function(..., type = "seq", palette = 1, direction = 1, aesthetics = "pattern_fill2") {
  discrete_scale(aesthetics, 
                 palette = brewer_pal(type, palette, direction),
                 ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_brewer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_colour_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = guide_colourbar(available_aes = "pattern_colour"), aesthetics = "pattern_colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  continuous_scale(aesthetics,
    palette = gradient_n_pal(brewer_pal(type, palette, direction)(7), values, space),
    na.value = na.value,
    guide = guide,
    ...)
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_brewer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = guide_colourbar(available_aes = "pattern_fill"), aesthetics = "pattern_fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  continuous_scale(aesthetics,
                   palette = gradient_n_pal(brewer_pal(type, palette, direction)(7), values, space),
                   na.value = na.value,
                   guide = guide,
                   ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_pattern_colour_brewer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_fill2_distiller <- function(..., type = "seq", palette = 1, direction = -1, values = NULL, space = "Lab", na.value = "grey50", guide = guide_colourbar(available_aes = "pattern_fill2"), aesthetics = "pattern_fill2") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  continuous_scale(aesthetics, 
                   palette = gradient_n_pal(brewer_pal(type, palette, direction)(7), values, space),
                   na.value = na.value,
                   guide = guide,
                   ...)
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_brewer
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_colour_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "pattern_colour") {
#   # warn about using a qualitative brewer palette to generate the gradient
#   type <- match.arg(type, c("seq", "div", "qual"))
#   if (type == "qual") {
#     warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
#   }
#   binned_scale(aesthetics, palette = binned_pal(brewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
# }
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @export
# #' @rdname scale_pattern_colour_brewer
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_fill_fermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "pattern_fill") {
#   type <- match.arg(type, c("seq", "div", "qual"))
#   if (type == "qual") {
#     warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
#   }
#   binned_scale(aesthetics, palette = binned_pal(brewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
# }
