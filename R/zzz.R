#' @import ggplot2 glue grid rlang scales vctrs
#' @importFrom lifecycle deprecated deprecate_soft
#' @importFrom stats setNames
#' @importFrom utils tail
#' @importFrom grDevices col2rgb dev.off png rgb
NULL

.onLoad <- function(libname, pkgname) {
  ggplot2::register_theme_elements(element_tree = pattern_palette_element_tree())
}
