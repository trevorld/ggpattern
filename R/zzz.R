#' @import scales
#' @importFrom utils tail
#' @importFrom grDevices col2rgb dev.off png rgb
NULL

deprecated <- function(new, old) {
    if (interactive())
        .Deprecated(new, old = old)
    invisible(NULL)
}

img_read_memoised <- img_read

.onLoad <- function(libname, pkgname) {
    if (requireNamespace("memoise"))
        img_read_memoised <<- memoise::memoise(img_read)

    ns <- asNamespace(pkgname)
    makeActiveBinding("magick_filter_names",
                      function() {
                        deprecated("magick::filter_types()",
                                    old = "magick_filter_names")
                        magick::filter_types()
                      }, ns)
    makeActiveBinding("magick_gravity_names",
                      function() {
                        deprecated("magick::gravity_types()",
                                    old = "magick_gravity_names")
                        magick::gravity_types()
                      }, ns)
    makeActiveBinding("magick_pattern_intensity_names",
                      function() {
                        deprecated("gridpattern::names_magick_intensity",
                                    old = "magick_pattern_intensity_names")
                        gridpattern::names_magick_intensity
                      }, ns)
    makeActiveBinding("magick_pattern_names",
                      function() {
                        deprecated("gridpattern::names_magick",
                                    old = "magick_pattern_names")
                        gridpattern::names_magick
                      }, ns)
    makeActiveBinding("magick_pattern_stripe_names",
                      function() {
                        deprecated("gridpattern::names_magick_stripe",
                                    old = "magick_pattern_stripe_names")
                        gridpattern::names_magick_stripe
                      }, ns)
    makeActiveBinding("placeholder_names",
                      function() {
                        deprecated("gridpattern::names_placeholder",
                                    old = "placeholder_names")
                        gridpattern::names_placeholder
                      }, ns)
}
