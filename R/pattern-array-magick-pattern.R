



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read a user specified filename as an image
#'
#' @inheritParams create_gradient_as_array
#'
#' @return array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_pattern_as_array <- function(width, height, params, legend) {

  type   <- check_default(as.character(params$pattern_type),
                          options = magick_pattern_names,
                          default = 'hexagons')

  scale  <- check_default(params$pattern_scale, default = 1, type = 'numeric')

  filter <- tolower(as.character(params$pattern_filter))
  filter <- check_default(filter, options = tolower(magick::filter_types()), default = 'box')

  colour <- as.character(params$pattern_fill)

  img <- create_magick_pattern_img_scaled(
    width  = width,
    height = height,
    type   = type,
    colour = colour,
    scale  = scale,
    filter = filter
  )

  convert_img_to_array(img)
}







if (FALSE) {
  library(ggplot2)

  df1 <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))


  ggplot(df1, aes(trt, outcome)) +
    geom_col_pattern(aes(fill=trt, pattern_type = trt, pattern_fill = trt),
                     fill = 'white',
                     pattern           = 'magick',
                     # pattern_type      = 'checkerboard',
                     # pattern_fill      = 'tomato',
                     # pattern_alpha     = 0.5,
                     pattern_scale = 2,
                     colour            = 'black') +
    theme_bw(15) +
    labs(title = "ggpattern::geom_col_pattern()") +
    # theme(legend.position = 'none') +
    # scale_pattern_type_discrete(choices = magick_pattern_names) +
    theme(legend.key.size = unit(2, 'cm')) +
    scale_pattern_type_discrete(choices = magick_pattern_intensity_names[2:20]) +
    coord_fixed(ratio = 1/2)




  ggplot(mtcars) +
    geom_density_pattern(
      aes(
        x             = mpg,
        pattern_fill  = as.factor(cyl),
        # pattern_alpha = as.factor(cyl),
        pattern_type  = as.factor(cyl)
      ),
      pattern      = 'magick',
      pattern_scale = 5
    ) +
    theme_bw(15) +
    # theme(legend.position = 'none') +
    theme(legend.key.size = unit(2, 'cm')) +
    labs(title = "ggpattern::geom_density_pattern()") +
    scale_pattern_type_discrete(choices = magick_pattern_names) +
    coord_fixed(ratio = 80)



  df2 <- data.frame(
    group = c("Cool", "But", "Use", "Less"),
    value = c(10, 20, 30, 40)
  )

  ggplot(df2, aes(x="", y = value, pattern_angle = group))+
      geom_bar_pattern(
        aes(pattern_type = group, pattern_fill = group),
        pattern = 'magick',
        pattern_scale = 4,
        width                = 1,
        stat                 = "identity",
        fill                 = 'white',
        colour               = 'black',
        pattern_aspect_ratio = 1,
        pattern_density      = 0.3
      ) +
      coord_polar("y", start=0) +
      theme_minimal(20) +
      theme(
        # legend.position = 'none',
        legend.key.size = unit(2.5, 'cm')
      ) +
      scale_pattern_type_discrete(choices = magick_pattern_stripe_names) +
      labs(title = "ggpattern::geom_bar_pattern() + coord_polar()")
}
