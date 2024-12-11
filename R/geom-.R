# * This is the list of all pattern aesthetics.
# * List is shared across every geom
# * Not all aesthetics are used by all patterns.
pattern_aesthetics <- aes(
  pattern                  = 'stripe',
  pattern_type             = NA,
  pattern_subtype          = NA,

  pattern_angle            = 30,
  pattern_density          = 0.2,
  pattern_spacing          = 0.05,
  pattern_xoffset          = 0,
  pattern_yoffset          = 0,

  pattern_alpha            = NA,
  pattern_linetype         = 1,
  pattern_size             = 0.5,
  pattern_shape            = 1,
  pattern_colour           = 'grey20',
  pattern_fill             = 'grey80',
  pattern_fill2            = NA,

  pattern_aspect_ratio     = NA,
  pattern_key_scale_factor = 1,

  pattern_filename         = '',
  pattern_gravity          = NA,   # magick::gravity_types()
  pattern_filter           = '',  # magick::filter_types()
  pattern_scale            = 1,
  pattern_orientation      = 'vertical',

  pattern_phase            = 0,
  pattern_frequency        = 0.1,

  pattern_option_1         = 0,
  pattern_option_2         = 0,
  pattern_option_3         = 0,
  pattern_option_4         = 0,
  pattern_option_5         = 0,

  pattern_grid             = 'square',
  pattern_rot              = 0,
  pattern_res              = getOption("ggpattern_res", NA),
  pattern_units            = 'snpc'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the patterned area to be used in the legend key
#' @inheritParams  draw_key_polygon_pattern
#' @param boundary_df the boundary of the pattern in npc coordinates
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_key_pattern_grob <- function(data, params, size, aspect_ratio, boundary_df) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # From 'draw_key_polygon', this sets default sizes if none given.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data$size <- data$linewidth %||% data$size %||% 0.5
  lwd <- min(data$size, min(size) / 4) * .pt

  if (data$pattern_units == "snpc") {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Convert the width/height of the key into npc sizes
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      key_native_x <- abs(as.numeric(grid::convertWidth (unit(size[1], 'mm'), 'native')))
      key_native_y <- abs(as.numeric(grid::convertHeight(unit(size[2], 'mm'), 'native')))

      vp <- grid::current.viewport()
      vp_native_x <- abs(diff(vp$xscale))
      vp_native_y <- abs(diff(vp$yscale))


      key_npc_x <- abs(as.numeric(grid::convertWidth (unit(size[1], 'mm'), 'npc')))
      key_npc_y <- abs(as.numeric(grid::convertHeight(unit(size[2], 'mm'), 'npc')))

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # What's the overall scale_factor?
      # The legend is actually drawn in its own viewport with an area of 1x1 npc.
      # I have to do some fancy scaling to draw the current pattern in this
      # scaled viewport as currently appears in the full viewport of the plot.
      # i.e. I need to make the pattern in the legend look like the pattern in the
      # plot.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      denom <- sqrt(2) * (1/aspect_ratio) * 9/8

      if (vp_native_x/vp_native_y < aspect_ratio) {
        scale_factor <- 1/key_npc_x / aspect_ratio / denom
      } else {
        scale_factor <- 1/key_npc_y/denom
      }

      scale_factor <- scale_factor * data$pattern_key_scale_factor

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Compensate for box the key is rendered in being different aspect ratios
      # i.e. theme(legend.key.width  = unit(2, 'cm'),
      #            legend.key.height = unit(3, 'cm')
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      key_aspect_ratio <- key_native_x/key_native_y
      scale_factor <- scale_factor / key_aspect_ratio
  } else {
      scale_factor <- 1.00 * data$pattern_key_scale_factor
  }

  this_params <- fill_default_params(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Scale the pattern parameters such that when they're drawn in the key,
  # they will look like what's drawn on the plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_params$pattern_spacing <- this_params$pattern_spacing * scale_factor
  this_params$pattern_xoffset <- this_params$pattern_xoffset * scale_factor
  this_params$pattern_yoffset <- this_params$pattern_yoffset * scale_factor

  if (is.null(this_params$pattern_res) || is.na(this_params$pattern_res)) {
    native <- as.numeric(grid::convertWidth(unit(1, "npc"), "native"))
    inches <- as.numeric(grid::convertWidth(unit(1, "npc"), "in"))
    this_params$pattern_res <- 1.3 * scale_factor * native / inches
  }

  gridpattern_pattern(this_params, boundary_df, key_aspect_ratio, legend = TRUE)
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

  # message("draw_key_polygon_pattern(): aspect_ratio = ", aspect_ratio)
  lwd <- min(data$linewidth %||% data$size, min(size) / 4) #* .pt

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Currently not sure, why, but the key_grob is drawn slightly undersized
  # with an offset of "lwd" mm.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  w <- grid::convertWidth (unit(lwd, "mm"), 'npc')
  h <- grid::convertHeight(unit(lwd, "mm"), 'npc')
  w <- as.numeric(w)
  h <- as.numeric(h)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is a boundary_df polygong the size of the key in npc coordinates in
  # it's own viewport. This is approximately a 1x1 npc unit square.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  boundary_df <- create_polygon_df(c(h, 1-h, 1-h, h), c(h, h, 1-h, 1-h))


  key_pattern_grob <- create_key_pattern_grob(data, params, size, aspect_ratio, boundary_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # `lineend` is a workaround for Windows and intentionally kept unexposed
  # as an argument. (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-457504667)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_grob_fn <- function(col, fill, lwd) {
      rectGrob(
        width  = unit(1, "npc"), #- unit(lwd, "mm"),
        height = unit(1, "npc"), #- unit(lwd, "mm"),
        gp = gpar(
          col      = col,
          fill     = fill,
          lty      = data$linetype %||% 1,
          lwd      = lwd,
          linejoin = params$linejoin %||% "mitre",
          lineend  = if (identical(params$linejoin, "round")) "round" else "square"
        ))
  }
  col <- data$colour %||% NA
  fill <- fill_alpha(data$fill %||% "grey20", data$alpha)
  key_grob_fill <- key_grob_fn(NA, fill, 0)
  key_grob_border <- key_grob_fn(col, NA, lwd)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble grob to return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    key_grob_fill,
    # verboseGrob("legend"),
    key_pattern_grob,
    key_grob_border
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname draw_key_polygon_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_boxplot_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the boundary_df for the rectangular region of the crossbar.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xmin <- 0.5 - 0.75/2
  xmax <- 0.5 + 0.75/2
  ymin <- 0.5 - 0.5 /2
  ymax <- 0.5 + 0.5 /2
  boundary_df <- create_polygon_df(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pattern in this region
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_pattern_grob <- create_key_pattern_grob(data, params, size, aspect_ratio, boundary_df)

  key_grob_box <- grobTree(
    rectGrob(height = 0.5, width = 0.75),
    gp = gpar(
      col = data$colour %||% "grey20",
      fill = fill_alpha(data$fill %||% "white", data$alpha),
      lwd = (data$linewidth %||% data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    )
  )


  key_grob_line <- grobTree(
    linesGrob(0.5, c(0.1, 0.25)),
    linesGrob(0.5, c(0.75, 0.9)),
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour %||% "grey20",
      fill = fill_alpha(data$fill %||% "white", data$alpha),
      lwd = (data$linewidth %||% data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble grob to return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    key_grob_box,
    key_pattern_grob,
    key_grob_line
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname draw_key_polygon_pattern
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_crossbar_pattern <- function(data, params, size, aspect_ratio = get_aspect_ratio()) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the boundary_df for the rectangular region of the crossbar.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  xmin <- 0.5 - 0.75/2
  xmax <- 0.5 + 0.75/2
  ymin <- 0.5 - 0.5 /2
  ymax <- 0.5 + 0.5 /2
  boundary_df <- create_polygon_df(c(xmin, xmax, xmax, xmin), c(ymin, ymin, ymax, ymax))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pattern in this region
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_pattern_grob <- create_key_pattern_grob(data, params, size, aspect_ratio, boundary_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generate the box and the line for the crossbar grob separately, as the
  # pattern goes in between the two
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  key_grob_box <- grobTree(
    rectGrob(height = 0.5, width = 0.75),
    gp = gpar(
      col = data$colour %||% "grey20",
      fill = fill_alpha(data$fill %||% "white", data$alpha),
      lwd = (data$linewidth %||% data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    )
  )

  key_grob_line <- grobTree(
    linesGrob(c(0.125, 0.875), 0.5),
    gp = gpar(
      col = data$colour %||% "grey20",
      fill = fill_alpha(data$fill %||% "white", data$alpha),
      lwd = (data$linewidth %||% data$size %||% 0.5) * .pt,
      lty = data$linetype %||% 1
    )
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Assemble grob to return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grobTree(
    key_grob_box,
    key_pattern_grob,
    key_grob_line
  )
}

check_linewidth <- function(data, name) {
  if (is.null(data$linewidth) && !is.null(data$size)) {
    lifecycle::deprecate_warn("1.0.1",
                              I(paste0("Using the `size` aesthetic with ", name)),
                              I("the `linewidth` aesthetic"))
    data$linewidth <- data$size
  }
  data
}
