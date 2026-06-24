library("grid")
library("ggplot2")
library("ggpattern")
library("piecepackr")

draw_logo <- function(bleed = FALSE, cut = FALSE) {
	angle <- seq(0, 2 * pi, length.out = 7) + pi / 6
	polygon_df <- data.frame(
		angle = angle,
		x = cos(angle),
		y = sin(angle)
	)
	p <- ggplot(polygon_df) +
		geom_polygon_pattern(
			aes(x = x, y = y),
			fill = "white",
			colour = "black",
			pattern_spacing = 0.15,
			pattern_density = 0.4,
			pattern_fill = "lightblue",
			pattern_colour = "#002366",
			pattern_angle = 45
		) +
		labs(title = "ggpattern") +
		coord_equal() +
		theme_bw(25) +
		theme(axis.title = element_blank())

	plot_grob <- ggplotGrob(p)

	w <- 4.5
	grid.newpage()
	pushViewport(viewport(width = unit(w, "inches"), height = unit(w, "inches")))
	hex <- pp_shape("convex6")

	# Draw hex background
	grid.draw(hex$shape(gp = gpar(fill = "white", col = "black", lwd = 4)))

	# Draw ggplot grob clipped to the hex shape via a mask
	hex_mask <- as.mask(hex$shape(gp = gpar(fill = "white", col = NA)))
	pushViewport(viewport(mask = hex_mask))
	pushViewport(viewport(x = 0.45, height = unit(0.8 * w, "inches")))
	grid.draw(plot_grob)
	popViewport()
	popViewport()

	# Redraw hex border on top to clean up edges
	grid.draw(hex$shape(gp = gpar(fill = NA, col = "black", lwd = 4)))

	if (isTRUE(bleed) && isTRUE(cut)) {
		pushViewport(viewport(width = unit(5 / 6, "npc"), height = unit(5 / 6, "npc")))
		grid.draw(hex$shape(gp = gpar(fill = "transparent", col = "orange")))
		popViewport()
	}

	popViewport()
}

w <- 4.5
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in", res = 72, bg = "transparent")
draw_logo()
dev.off()
