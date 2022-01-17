#' @section Package options:
#' The following `ggpattern` options may be set globally via [base::options()]:
#'  \describe{
#'    \item{ggpattern_array_funcs}{Set custom \dQuote{array} pattern functions.}
#'    \item{ggpattern_geometry_funcs}{Set custom \dQuote{geometry} pattern functions.}
#'    \item{ggpattern_res}{Set new default raster resolution default (pixels per inch)
#'                         for the `pattern_res` aesthetic.}
#'    \item{ggpattern_use_R4.1_clipping}{If `TRUE` use the grid clipping path feature introduced in R v4.1.0
#'                          else do a `rasterGrob` approximation of the clipped pattern.}
#'    \item{ggpattern_use_R4.1_features}{If `TRUE` sets the default for all the other
#'                          `ggpattern_use_R4.1_*` options arguments to `TRUE`.}
#'    \item{ggpattern_use_R4.1_gradients}{If `TRUE` use the grid gradient feature introduced in R v4.1.0
#'                          else do a `rasterGrob` approximation of the gradient pattern.}
#'    \item{ggpattern_use_R4.1_masks}{If `TRUE` use the grid mask feature introduced in R v4.1.0.
#'                          else do a `rasterGrob` approximation of the masked pattern.}
#'    \item{ggpattern_use_R4.1_patterns}{If `TRUE` use the grid pattern feature introduced in R v4.1.0.
#'                          Available for use in writing custom patterns.}
#'  }
#'  Note to use the R v4.1.0 features one needs R be (at least) version 4.1 and not all graphic devices
#'  support any/all these features.  See \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html} for more information on the R v4.1.0 grid features.
#' @keywords internal
#' @name ggpattern
"_PACKAGE"
