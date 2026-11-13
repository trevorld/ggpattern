#' @export
#' @rdname geom-docs
geom_crossbar_pattern <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
                                  middle.colour = NULL,
                                  middle.color = NULL,
                                  middle.linetype = NULL,
                                  middle.linewidth = NULL,
                                  box.colour = NULL,
                                  box.color = NULL,
                                  box.linetype = NULL,
                                  box.linewidth = NULL,
                                  fatten = deprecated(),
                                  na.rm = FALSE,
                                  orientation = NA,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {

  middle_gp <- list(
    colour = middle.color %||% middle.colour,
    linetype = middle.linetype,
    linewidth = middle.linewidth
  )

  box_gp <- list(
    colour = box.color %||% box.colour,
    linetype = box.linetype,
    linewidth = box.linewidth
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrossbarPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      middle_gp = middle_gp,
      box_gp = box_gp,
      fatten = fatten,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCrossbarPattern <- ggproto("GeomCrossbarPattern", GeomCrossbar,


  default_aes = defaults(aes(
		colour = from_theme(colour %||% ink),
		fill = from_theme(fill %||% NA),
		linewidth = from_theme(borderwidth),
		linetype = from_theme(bordertype),
		alpha = NA
	),
		pattern_aesthetics
  ),

  # {covr} doesn't cover `draw_key_crossbar_pattern()` if directly assign it
  draw_key = function(data, params, size) draw_key_crossbar_pattern(data, params, size),

  draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "mitre", fatten = 2.5, width = NULL,
                        flipped_aes = FALSE, middle_gp = NULL, box_gp = NULL) {
    grob <- ggproto_parent(GeomCrossbar, self)$draw_panel(
      data, panel_params, coord, lineend = lineend, linejoin = linejoin,
      fatten = fatten, width = width, flipped_aes = flipped_aes,
      middle_gp = middle_gp, box_gp = box_gp
    )
    # one box polygon per row (the middle segments have no area)
    ggname("geom_crossbar_pattern", with_pattern_aes(grob, data))
  }
)
