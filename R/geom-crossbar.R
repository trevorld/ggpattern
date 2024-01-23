#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_crossbar_pattern <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
                                  fatten = 2.5,
                                  na.rm = FALSE,
                                  orientation = NA,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrossbarPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomCrossbarPattern <- ggproto(
  "GeomCrossbarPattern", GeomCrossbar,

  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      colour   = "black",
      fill     = NA,
      linewidth= 0.5,
      linetype = 1,
      alpha    = NA
    )
  ),

  draw_key = function(self, ...) draw_key_crossbar_pattern(...),

  draw_panel = function(self, data, panel_params, coord, fatten = 2.5, width = NULL, flipped_aes = FALSE) {

    data <- flip_data(data, flipped_aes)

    middle <- transform(data, x = xmin, xend = xmax, yend = y, linewidth = linewidth * fatten, alpha = NA)

    has_notch <- !is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
      !is.na(data$ynotchlower) && !is.na(data$ynotchupper)

    if (has_notch) {
      if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
        message("notch went outside hinges. Try setting notch=FALSE.")

      notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

      middle$x <- middle$x + notchindent
      middle$xend <- middle$xend - notchindent

      box <- new_data_frame(list(
        x = c(
          data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
          data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
          data$xmin
        ),
        y = c(
          data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
          data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
          data$ymax
        ),
        alpha    = rep(data$alpha, 11),
        colour   = rep(data$colour, 11),
        linewidth     = rep(data$linewidth, 11),
        linetype = rep(data$linetype, 11),
        fill     = rep(data$fill, 11),
        group    = rep(seq_len(nrow(data)), 11)
      ))

      # Copy across all the pattern aesthetics
      for (varname in names(pattern_aesthetics)) {
        box[[varname]] <- data[[varname]]
      }
    } else {
      # No notch
      box <- new_data_frame(list(
        x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
        y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
        alpha    = rep(data$alpha, 5),
        colour   = rep(data$colour, 5),
        linewidth     = rep(data$linewidth, 5),
        linetype = rep(data$linetype, 5),
        fill     = rep(data$fill, 5),
        group    = rep(seq_len(nrow(data)), 5)
      ))

      # Copy across all the pattern aesthetics
      for (varname in names(pattern_aesthetics)) {
        box[[varname]] <- data[[varname]]
      }
    }
    box <- flip_data(box, flipped_aes)
    middle <- flip_data(middle, flipped_aes)

    ggname("geom_crossbar", gTree(children = gList(
      GeomPolygonPattern$draw_panel(box, panel_params, coord),
      GeomSegment$draw_panel(middle, panel_params, coord)
    )))
  },
  rename_size = TRUE
)
