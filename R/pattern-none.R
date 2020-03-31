

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create grob objects for the pattern elements within a boundary
#'
#' @param params params/coords for a single element. named list or single row data.frame
#' @param boundary_df mask for the pattern rendering
#' @param aspect_ratio a aspect ratio of the plotting area.
#' @param legend is the pattern being created in the legend? default FALSE.
#'  Use this flag if you want different pattern drawing behviour for the legend.
#'
#' @return grid grob objects.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_none <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
  grid::nullGrob()
}



if (FALSE) {

  library(ggplot2)
  library(dplyr)
  library(ggpattern)

  plot_df <- data.frame(
    xmin    = c(0, 10),
    xmax    = c(8, 18),
    ymin    = c(0, 10),
    ymax    = c(5, 19),
    type    = c('a', 'b'),
    angle   = c(45, 0),
    pname   = c('crosshatch', 'circle'),
    pcolour = c('red', 'blue'),
    pspace  = c(0.03, 0.05),
    stringsAsFactors = FALSE
  )


  ggplot(plot_df) +
    geom_rect_pattern(
      aes(
        xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
        pattern = type
      ),
      fill            = 'white',
      colour          = 'black',
      pattern_density = 0.1
    ) +
    theme_bw() +
    scale_pattern_manual(values = c(b='stripe', a='none')) +
    labs(title = "ggpattern::geom_rect_pattern()")
}
