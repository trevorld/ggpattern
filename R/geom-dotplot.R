#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDotplotPattern <- ggproto("GeomDotplotPattern", GeomDotplot,
  default_aes = defaults(aes(
    colour = from_theme(colour %||% ink),
    fill = from_theme(fill %||% ink),
    alpha = NA,
    stroke = from_theme(borderwidth * 2),
    linetype = from_theme(linetype),
    weight = 1,
    width = 0.9
  ),
    pattern_aesthetics
  ),

  draw_group = function(self, data, panel_params, coord, lineend = "butt", na.rm = FALSE,
                        binaxis = "x", stackdir = "up", stackratio = 1,
                        dotsize = 1, stackgroups = FALSE) {
    grob <- ggproto_parent(GeomDotplot, self)$draw_group(
      data, panel_params, coord, lineend = lineend, na.rm = na.rm,
      binaxis = binaxis, stackdir = stackdir, stackratio = stackratio,
      dotsize = dotsize, stackgroups = stackgroups
    )
    # one dot per row
    # (the dots of the `dotstackGrob` are only made when it is drawn so they are filled then)
    ggname("geom_dotplot_pattern", with_pattern_aes(grob, data))
  },

  draw_key = function(data, params, size) {
    with_pattern_key(ggplot2::draw_key_dotplot(data, params, size), data, size)
  }
)

#' @rdname geom-docs
#' @export
geom_dotplot_pattern <- function(mapping = NULL, data = NULL,
                                 position = "identity",
                                 ...,
                                 binwidth = NULL,
                                 binaxis = "x",
                                 method = "dotdensity",
                                 binpositions = "bygroup",
                                 stackdir = "up",
                                 stackratio = 1,
                                 dotsize = 1,
                                 stackgroups = FALSE,
                                 origin = NULL,
                                 right = TRUE,
                                 width = 0.9,
                                 drop = FALSE,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  # If identical(position, "stack") or position is position_stack(), tell them
  # to use stackgroups and set position to "identity"
  if (!is.null(position) &&
      (identical(position, "stack") || (inherits(position, "PositionStack")))) {
    cli::cli_inform(
      "{.code position = \"stack\"} doesn't work properly with {.fn geom_dotplot_pattern}. Use {.code stackgroups = TRUE} instead."
    )
  }

  if (stackgroups && method == "dotdensity" && binpositions == "bygroup") {
    cli::cli_inform(c(
      "{.fn geom_dotplot_pattern} called with {.code stackgroups = TRUE} and {.code method = \"dotdensity\"}.",
      i = "Do you want {.code binpositions = \"all\"} instead?"
    ))
  }

  stackdir <- arg_match0(stackdir, c("up", "down", "center", "centerwhole"), "stackdir")
  layer(
    data = data,
    mapping = mapping,
    stat = StatBindot,
    geom = GeomDotplotPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    # Need to make sure that the binaxis goes to both the stat and the geom
    params = list2(
      binaxis = binaxis,
      binwidth = binwidth,
      binpositions = binpositions,
      method = method,
      origin = origin,
      right = right,
      width = width,
      drop = drop,
      stackdir = stackdir,
      stackratio = stackratio,
      dotsize = dotsize,
      stackgroups = stackgroups,
      na.rm = na.rm,
      ...
    )
  )
}
