#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_ribbon_pattern <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE,
                                outline.type = "both") {
  outline.type <- match.arg(outline.type, c("both", "upper", "legacy"))

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomRibbonPattern,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm        = na.rm,
      orientation  = orientation,
      outline.type = outline.type,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomRibbonPattern <- ggproto(
  "GeomRibbonPattern", GeomRibbon,
  default_aes = augment_aes(
    pattern_aesthetics,
    ggplot2::aes(
      colour   = NA,
      fill     = "grey20",
      linewidth     = 0.5,
      linetype = 1,
      alpha    = NA
    )
  ),

  aspect_ratio = 1,

  draw_key = function(self, ...) {
    draw_key_polygon_pattern(..., aspect_ratio = self$aspect_ratio)
  },

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Where the magic happens
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  draw_group = function(self, data, panel_params, coord, na.rm = FALSE, flipped_aes = FALSE, outline.type = "both") {
    data <- ggplot2::flip_data(data, flipped_aes)
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]

    # Check that aesthetics are constant
    aes_names <- c(
      "colour", "fill", "linewidth", "linetype", "alpha",
      names(pattern_aesthetics)
    )


    aes <- unique(data[aes_names])
    if (nrow(aes) > 1) {
      abort("Aesthetics can not vary with a ribbon")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    data <- unclass(data) #for faster indexing
    positions <- new_data_frame(list(
      x = c(data$x, rev(data$x)),
      y = c(data$ymax, rev(data$ymin)),
      id = c(ids, rev(ids))
    ))

    positions <- ggplot2::flip_data(positions, flipped_aes)

    munched <- coord_munch(coord, positions, panel_params)

    g_poly <- polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = gpar(
        fill = scales::alpha(aes$fill, aes$alpha),
        col  = if (identical(outline.type, "legacy")) aes$colour else NA
      )
    )


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate all the boundary_dfs for all the elements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    stopifnot(!is.null(munched$id))

    polygons <- split(munched, munched$id)
    boundary_dfs <- lapply(polygons, function(polygon) {
      create_polygon_df(
        x = polygon$x,
        y = polygon$y
      )
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # For polygons, every row in first_rows represents an element.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    first_idx  <- !duplicated(munched$id)
    first_rows <- munched[first_idx, ]
    all_params <- first_rows
    all_params <- cbind(all_params, aes)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create the pattern grobs given the current params for every element
    # (given in all_params), and the boundary_dfs of all the elements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    self$aspect_ratio  <- get_aspect_ratio()
    pattern_grobs <- create_pattern_grobs(all_params, boundary_dfs, self$aspect_ratio)

    if (identical(outline.type, "legacy")) {
      warn(glue('outline.type = "legacy" is only for backward-compatibility ',
                'and might be removed eventually'))
      return(ggname("geom_ribbon", grobTree(g_poly, pattern_grobs)))
    }

    munched_lines <- munched
    # increment the IDs of the lower line
    munched_lines$id <- switch(
      outline.type,
      both = munched_lines$id + rep(c(0, max(ids, na.rm = TRUE)), each = length(ids)),
      upper = munched_lines$id + rep(c(0, NA), each = length(ids)),
      abort(glue("invalid outline.type: {outline.type}"))
    )
    g_lines <- polylineGrob(
      munched_lines$x, munched_lines$y, id = munched_lines$id,
      default.units = "native",
      gp = gpar(
        col = aes$colour,
        lwd = aes$linewidth * .pt,
        lty = aes$linetype)
    )



    ggname("geom_ribbon", grobTree(g_poly, pattern_grobs, g_lines))
  },

  rename_size = TRUE

)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom-docs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_area_pattern <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "stack", na.rm = FALSE, orientation = NA,
                              show.legend = NA, inherit.aes = TRUE, ...,
                              outline.type = "upper") {
  outline.type <- match.arg(outline.type, c("both", "upper", "legacy"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaPattern,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggpattern-ggproto
#' @format NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomAreaPattern <- ggproto(
  "GeomAreaPattern", GeomRibbonPattern,
  default_aes = augment_aes(
    pattern_aesthetics,
    aes(
      colour    = NA,
      fill      = "grey20",
      linewidth = 0.5,
      linetype  = 1,
      alpha     = NA
    )
  ),

  required_aes = c("x", "y"),

  setup_params = function(data, params) {
    params$flipped_aes <- ggplot2::has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
    ggplot2::flip_data(data, params$flipped_aes)
  },

  rename_size = TRUE
)
