# The values that `scale` (or if `NULL` the default scale) maps `values` of `aesthetic` to
# in a plot (with `theme` added)
scale_values <- function(scale, aesthetic, values, theme = NULL) {
    df <- data.frame(x = seq_along(values), y = 1, value = values)
    mapping <- ggplot2::aes(x, y)
    mapping[[aesthetic]] <- rlang::quo(value)
    p <- ggplot2::ggplot(df, mapping) + geom_col_pattern() + scale + theme
    ggplot2::layer_data(p)[[aesthetic]]
}
