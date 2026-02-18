#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomRibbonPattern <- ggproto("GeomRibbonPattern", GeomRibbon,
  default_aes = defaults(aes(
	colour = from_theme(colour %||% NA),
    fill = from_theme(fill %||% col_mix(ink, paper, 0.2)),
    linewidth = from_theme(borderwidth),
	linetype = from_theme(bordertype),
    alpha = NA
	),
    pattern_aesthetics
  ),

  draw_key = draw_key_polygon_pattern,

  draw_group = function(self, data, panel_params, coord, lineend = "butt",
                        linejoin = "round", linemitre = 10, na.rm = FALSE,
                        flipped_aes = FALSE, outline.type = "both") {
    data <- fix_linewidth(data, snake_class(self))
    data <- flip_data(data, flipped_aes)
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]

    # Check that aesthetics are constant
    aes <- unique0(data[names(data) %in% c("colour", "fill", "linewidth", "linetype", "alpha", names(pattern_aesthetics))])
    if (nrow(aes) > 1) {
      cli::cli_abort("Aesthetics can not vary along a ribbon.")
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

    # In case the data comes from stat_align
    upper_keep <- TRUE
    if (!is.null(data$align_padding)) {
      upper_keep <- !data$align_padding
      # `align_padding` can be NA when group is the only group in panel
      upper_keep[is.na(upper_keep)] <- TRUE
    }

    # The upper line and lower line need to processed separately (#4023)
    positions_upper <- data_frame0(
      x = data$x[upper_keep],
      y = data$ymax[upper_keep],
      id = ids[upper_keep]
    )

    positions_lower <- data_frame0(
      x = rev(data$x),
      y = rev(data$ymin),
      id = rev(ids)
    )

    positions_upper <- flip_data(positions_upper, flipped_aes)
    positions_lower <- flip_data(positions_lower, flipped_aes)

    munched_upper <- coord_munch(coord, positions_upper, panel_params)
    munched_lower <- coord_munch(coord, positions_lower, panel_params)

    munched_poly <- vec_rbind(munched_upper, munched_lower)

    is_full_outline <- identical(outline.type, "full")
    g_poly_fn <- function(col, fill, lwd) { polygonGrob(
      munched_poly$x, munched_poly$y, id = munched_poly$id,
      default.units = "native",
      gp = gpar(
        col = col,
        fill = fill,
        lwd = lwd,
        lty = if (is_full_outline) aes$linetype else 1,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )}
    g_poly_fill <- g_poly_fn(NA, fill_alpha(aes$fill, aes$alpha), 0)

    stopifnot(!is.null(munched_poly$id))
    polygons <- split(munched_poly, munched_poly$id)
    boundary_dfs <- lapply(polygons, function(polygon) {
      create_polygon_df(
        x = polygon$x,
        y = polygon$y
      )
    })

    first_idx  <- !duplicated(munched_poly$id)
    first_rows <- munched_poly[first_idx, ]
    all_params <- cbind(first_rows, aes)
    pattern_grobs <- create_pattern_grobs(all_params, boundary_dfs)



    if (is_full_outline) {
      col <- if (is_full_outline) aes$colour else NA
      lwd <- if (is_full_outline) aes$linewidth * .pt else 0
      g_poly_border <- g_poly_fn(col, NA, lwd)
      ggname("geom_ribbon_pattern", grobTree(g_poly_fill, pattern_grobs, g_poly_border))
    } else {
      # Increment the IDs of the lower line so that they will be drawn as separate lines
      munched_lower$id <- munched_lower$id + max(ids, na.rm = TRUE)

      munched_lines <- switch(outline.type,
        both = vec_rbind(munched_upper, munched_lower),
        upper = munched_upper,
        lower = munched_lower
      )
      g_lines <- polylineGrob(
        munched_lines$x, munched_lines$y, id = munched_lines$id,
        default.units = "native",
        gp = gpar(
          col = aes$colour,
          lwd = aes$linewidth * .pt,
          lty = aes$linetype,
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )
      ggname("geom_ribbon_pattern", grobTree(g_poly_fill, pattern_grobs, g_lines))
    }
  },

  rename_size = TRUE
)

#' @rdname ggpattern-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAreaPattern <- ggproto("GeomAreaPattern", GeomRibbonPattern,

	required_aes = c("x", "y"),

	setup_params = GeomArea$setup_params,

	setup_data = GeomArea$setup_data
)

#' @rdname geom-docs
#' @export
geom_ribbon_pattern <- make_constructor(
  GeomRibbonPattern, orientation = NA,
  checks = exprs(
    outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))
  )
)

#' @rdname geom-docs
#' @export
geom_area_pattern <- make_constructor(
  GeomAreaPattern, stat = "align", position = "stack",
  orientation = NA, outline.type = "upper",
  checks = exprs(
    outline.type <- arg_match0(outline.type, c("both", "upper", "lower", "full"))
  )
)
