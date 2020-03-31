


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read a user specified filename/URL as an image
#'
#' @inheritParams create_gradient_as_array
#'
#' @return array
#'
#' @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
img_read_as_array_wrapper <- function(width, height, params, legend) {

  filename <- as.character(params$pattern_filename)

  fill_type <- tolower(as.character(params$pattern_type))
  fill_type <- check_default(fill_type, options = fill_types)

  gravity <- tolower(as.character(params$pattern_gravity))
  gravity <- check_default(gravity, tolower(magick::gravity_types()), 'center')

  filter <- tolower(as.character(params$pattern_filter))
  filter <- check_default(filter, tolower(magick::filter_types()), 'lanczos')

  scale  <- params$pattern_scale
  scale  <- check_default(scale, default = 1, type = 'numeric')

  img_read_as_array(
    filename    = filename,
    width       = width,
    height      = height,
    fill_type   = fill_type,
    gravity     = gravity,
    filter      = filter,
    scale       = scale
  )
}







if (FALSE) {
  library(ggplot2)

  logo_filename  <- system.file("img", "Rlogo.png", package="png")
  rand1_filename <- '/Users/mike/Desktop/video/280px-PNG_transparency_demonstration_1.png'
  rand2_filename <- '/Users/mike/Desktop/screenshots/bird.jpg'

  df1 <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2),
                    scale = c(1, 1, -1),
                    filename = c(logo_filename, rand1_filename, rand2_filename), stringsAsFactors = FALSE)

  # logo_filename <- "crap.png"

  ggplot(df1, aes(trt, outcome)) +
    geom_col_pattern(
      aes(
        fill             = trt,
        pattern_filename = trt
      ),
      # fill = 'white',
      pattern           = 'image',
      pattern_type      = 'tile',
      # pattern_filename  = logo_filename,
      pattern_scale = -1,
      colour = 'black'
    ) +
    theme_bw(15) +
    labs(title = "ggpattern::geom_density_pattern() with image") +
    # theme(legend.position = 'none') +
    theme(legend.key.size = unit(2, 'cm')) +
    # scale_pattern_filename_identity(guide = 'legend') +
    scale_pattern_filename_manual(values = c(
      a = logo_filename,
      b = rand1_filename,
      c = rand2_filename
    )) +
    coord_fixed(ratio = 1/2)


  ggplot(df1, aes(trt, outcome)) +
    geom_col_pattern(
      aes(
        pattern_filename = trt
      ),
      fill = 'white',
      pattern           = 'image',
      pattern_type      = 'tile',
      # pattern_filename  = logo_filename,
      pattern_scale = -1,
      colour = 'black'
    ) +
    theme_bw(15) +
    labs(title = "ggpattern::geom_density_pattern() with image") +
    # theme(legend.position = 'none') +
    theme(legend.key.size = unit(2, 'cm')) +
    # scale_pattern_filename_identity() +
    labs(fill = "fill", pattern_filename = "fill") +
    coord_fixed(ratio = 1)













  ggplot(mtcars) +
    geom_density_pattern(
      aes(
        x             = mpg,
        pattern_fill  = as.factor(cyl),
        pattern_filename = as.factor(cyl),
        pattern_alpha = as.factor(cyl)
      ),
      pattern          = 'image',
      # pattern_filename = rand2_filename,
      pattern_type     = 'squish'
    ) +
    theme_bw(15) +
    # theme(legend.position = 'none') +
    theme(legend.key.size = unit(2, 'cm')) +
    labs(title = "ggpattern::geom_density_pattern() with image") +
    coord_fixed(ratio = 80)





  df2 <- data.frame(
    group = c("Cool", "But", "Use", "Less"),
    value = c(10, 20, 30, 40)
  )

  ggplot(df2, aes(x="", y = value, pattern_angle = group))+
    geom_bar_pattern(
      pattern              = 'image',
      pattern_filename     = rand1_filename,
      width                = 1,
      stat                 = "identity",
      fill                 = 'white',
      colour               = 'black',
      pattern_aspect_ratio = 1,
      pattern_density      = 0.3
    ) +
    coord_polar("y", start=0) +
    theme_void(20) +
    theme(
      legend.position = 'none',
      legend.key.size = unit(2.5, 'cm')
    ) +
    labs(title = "ggpattern::geom_bar_pattern() + coord_polar()")
}
