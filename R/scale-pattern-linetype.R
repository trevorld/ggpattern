#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Scale for line patterns
#'
#' Default line types based on a set supplied by Richard Pearson,
#' University of Manchester. Continuous values can not be mapped to
#' line types.
#'
#' @param ... see [ggplot2::scale_linetype()] for more information
#' @param na.value The linetype to use for `NA` values.
#' @return A [ggplot2::Scale] object.
#' @examples
#'   if (require("ggplot2")) {
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level, pattern_linetype = level),
#'         pattern_density = 0.6,
#'         pattern_size = 1.5,
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black',
#'         linewidth = 1.5
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       scale_pattern_linetype() +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'stripe'"
#'       )
#'     plot(gg)
#'   }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype <- function(..., na.value = "blank") {
  discrete_scale("pattern_linetype", 
                 palette = linetype_pal(),
                 na.value = na.value,
                 ...)
}

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #' @rdname scale_pattern_linetype
# #' @export
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scale_pattern_linetype_binned <- function(..., na.value = "blank") {
#   binned_scale("pattern_linetype", palette = binned_pal(linetype_pal()), ...)
# }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_linetype
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype_continuous <- function(...) {
  abort("A continuous variable can not be mapped to pattern_linetype")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_pattern_linetype
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_pattern_linetype_discrete <- scale_pattern_linetype
