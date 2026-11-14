# * This is the list of all pattern aesthetics.
# * List is shared across every geom
# * Not all aesthetics are used by all patterns.
# * `NaN` means "unset" i.e. `{gridpattern}` fills in the pattern's default (or else its generic default)
#   (unlike `NA` which may be meaningful e.g. a transparent colour or a "pch" shape that draws nothing).
pattern_aesthetics <- aes(
  pattern                  = 'stripe',
  pattern_type             = NaN,
  pattern_subtype          = NaN,

  pattern_angle            = NaN,
  pattern_density          = NaN,
  pattern_spacing          = NaN,
  pattern_xoffset          = NaN,
  pattern_yoffset          = NaN,

  pattern_alpha            = NaN,
  pattern_linetype         = NaN,
  pattern_linewidth        = NaN,
  pattern_size             = NaN,
  pattern_shape            = NaN,
  pattern_colour           = NaN,
  pattern_fill             = NaN,
  pattern_fill2            = NaN,

  pattern_aspect_ratio     = NaN,
  pattern_key_scale_factor = NaN,

  pattern_filename         = NaN,
  pattern_gravity          = NaN,   # magick::gravity_types()
  pattern_filter           = NaN,   # magick::filter_types()
  pattern_scale            = NaN,
  pattern_orientation      = NaN,

  pattern_phase            = NaN,
  pattern_frequency        = NaN,

  pattern_option_1         = NaN,
  pattern_option_2         = NaN,
  pattern_option_3         = NaN,
  pattern_option_4         = NaN,
  pattern_option_5         = NaN,

  pattern_grid             = NaN,
  pattern_rot              = NaN,
  pattern_res              = NaN,
  pattern_units            = NaN
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The default pattern aesthetics
#'
#' `default_pattern_aes()` returns the default values of the pattern aesthetics
#' (e.g. `pattern` and `pattern_fill`) for the `default_aes` of a pattern geom.
#' Most are `NaN` which means "unset":
#' the pattern's own (registered) default applies (or else `{gridpattern}`'s generic default)
#' unlike `NA` which may be meaningful (e.g. a transparent colour).
#' The only exception is `pattern` which defaults to `"stripe"`.
#'
#' @return A [ggplot2::aes()] mapping of constant values.
#' @seealso [make_GeomPattern()] which adds them to a geom's `default_aes` for you.
#'   See `vignette("developing-pattern-geoms", package = "ggpattern")` for writing a pattern geom.
#' @examples
#' names(default_pattern_aes())
#' # e.g. for the `default_aes` of a pattern geom made from `ggplot2::GeomSmooth`
#' default_aes <- ggplot2::aes(!!!ggplot2::GeomSmooth$default_aes, !!!default_pattern_aes())
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
default_pattern_aes <- function() pattern_aesthetics

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The numeric `legend` (zoom) of the pattern of a legend key
#'
#' So the pattern of a legend key looks like the pattern drawn on the plot.
#' @inheritParams  draw_key_polygon_pattern
#' @return A number (`NA` for a zero-size key).
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
key_legend_factor <- function(size, aspect_ratio) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the width/height of the key into npc sizes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vp <- grid::current.viewport()
  vp_native_x <- abs(diff(vp$xscale))
  vp_native_y <- abs(diff(vp$yscale))

  key_npc_x <- abs(as.numeric(grid::convertWidth (unit(size[1], 'mm'), 'npc')))
  key_npc_y <- abs(as.numeric(grid::convertHeight(unit(size[2], 'mm'), 'npc')))

  # Nothing to draw in a zero-size key (and its scale factor would be infinite)
  if (!isTRUE(key_npc_x > 0 && key_npc_y > 0)) {
    return(NA_real_)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What's the overall (geometric) scale factor?
  # The legend is actually drawn in its own viewport with an area of 1x1 npc.
  # I have to do some fancy scaling to draw the current pattern in this
  # scaled viewport as currently appears in the full viewport of the plot.
  # i.e. I need to make the pattern in the legend look like the pattern in the
  # plot.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  denom <- sqrt(2) * 9/8

  if (vp_native_x/vp_native_y < aspect_ratio) {
    geometric_factor <- 1 / (key_npc_x * denom)
  } else {
    geometric_factor <- aspect_ratio / (key_npc_y * denom)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compensate for box the key is rendered in being different aspect ratios
  # i.e. theme(legend.key.width  = unit(2, 'cm'),
  #            legend.key.height = unit(3, 'cm')
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_aspect_ratio <- (key_npc_x * vp_native_x) / (key_npc_y * vp_native_y)
  geometric_factor <- geometric_factor / key_aspect_ratio

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # A numeric `legend` makes `{gridpattern}` draw the pattern in a viewport
  # that many times larger than the key so relative ('snpc') lengths
  # look like what's drawn on the plot.
  # `{gridpattern}` additionally zooms the key by `pattern_key_scale_factor`
  # (for absolute units by scaling the spacing and offsets)
  # and scales an unset `pattern_res` to match.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  geometric_factor
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [ggplot2::layer()] or examples below.)
#'
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @param aspect_ratio the geom's best guess at what the aspect ratio might be.
#'
#' @return A grid grob.
#' @examples
#'   if (require("ggplot2")) {
#'
#'     # 'stripe' pattern example
#'     df <- data.frame(level = c("a", "b", "c", 'd'), outcome = c(2.3, 1.9, 3.2, 1))
#'     gg <- ggplot(df) +
#'       geom_col_pattern(
#'         aes(level, outcome, pattern_fill = level),
#'         pattern = 'stripe',
#'         fill    = 'white',
#'         colour  = 'black',
#'         key_glyph = draw_key_polygon_pattern
#'       ) +
#'       theme_bw(18) +
#'       theme(legend.position = 'none') +
#'       labs(
#'         title    = "ggpattern::geom_col_pattern()",
#'         subtitle = "pattern = 'stripe'"
#'       )
#'     plot(gg)
#'   }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_polygon_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {
  with_pattern_key(ggplot2::draw_key_polygon(data, params, size), data, size, aspect_ratio)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname draw_key_polygon_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_boxplot_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {
  with_pattern_key(ggplot2::draw_key_boxplot(data, params, size), data, size, aspect_ratio)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname draw_key_polygon_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_crossbar_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {
  with_pattern_key(ggplot2::draw_key_crossbar(data, params, size), data, size, aspect_ratio)
}
