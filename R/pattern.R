get_aspect_ratio <- function() {
    width <- as.numeric(convertWidth(unit(1, "npc"), "in"))
    height <- as.numeric(convertHeight(unit(1, "npc"), "in"))
    width / height
}

# The first row of each group of `data` (in the order {ggplot2} draws its groups' polygons)
first_rows_by_group <- function(data) {
    data <- data[!duplicated(data$group), , drop = FALSE]
    data[order(data$group), , drop = FALSE]
}
