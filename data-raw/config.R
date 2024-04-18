ggpattern_aes <-readr::read_delim(
"aes_name                | ggplot_name | aes_type   | scale_default
pattern                  |             | discrete   | c('stripe', 'crosshatch', 'circle')
pattern_type             |             | discrete   | NULL
pattern_subtype          |             | discrete   | NULL

pattern_angle            |             | continuous | c(0, 90)
pattern_density          |             | continuous | c(0, 0.5)
pattern_spacing          |             | continuous | c(0.01, 0.1)
pattern_xoffset          |             | continuous | c(0.01, 0.1)
pattern_yoffset          |             | continuous | c(0.01, 0.1)

pattern_alpha            | alpha       | continuous | c(0.1, 1)
pattern_linetype         | linetype    | discrete   | NULL
pattern_size             | size        | continuous | NULL
pattern_shape            | shape       | continuous | NULL
pattern_colour           | colour      | discrete   | NULL
pattern_fill             | fill        | discrete   | NULL
pattern_fill2            | fill        | discrete   | NULL

pattern_aspect_ratio     |             | continuous | c(0.5, 2)
pattern_key_scale_factor |             | continuous | c(0.5, 2)

pattern_filename         |             | discrete   | NULL
pattern_filter           |             | discrete   | c('lanczos', 'box', 'spline', 'cubic')
pattern_gravity          |             | discrete   | c('center', 'north', 'south', 'east', 'west', 'northeast', 'northwest', 'southeast', 'southwest')
pattern_scale            |             | continuous | c(0.5, 2)
pattern_orientation      |             | discrete   | c('horizontal', 'vertical', 'radial')

pattern_phase            |             | continuous | NULL
pattern_frequency        |             | continuous | NULL

pattern_grid             |             | discrete   | c('square', 'hex')
pattern_res              |             | continuous | NULL
pattern_rot              |             | continuous | c(0, 360)
", trim_ws = TRUE, delim = "|")
