test_that("`geom_rect_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("rect", {
        plot_df <- data.frame(
          xmin    = c(0, 10),
          xmax    = c(8, 18),
          ymin    = c(0, 10),
          ymax    = c(5, 19),
          type    = c('a', 'b'),
          angle   = c(45, 0),
          pname   = c('circle', 'circle'),
          pcolour = c('red', 'blue'),
          pspace  = c(0.03, 0.05),
          size    = c(0.5, 1),
          stringsAsFactors = FALSE
        )

        ggplot(plot_df) +
          geom_rect_pattern(
            aes(
              xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax,
              pattern_angle   = I(angle),
              pattern_colour  = I(pcolour),
              pattern_spacing = I(pspace),
              pattern_size    = I(size)
            ),
            pattern         = 'circle',
            fill            = 'white',
            colour          = 'black',
            pattern_density = 0.3
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_rect_pattern()") +
          theme(legend.key.size = unit(1.5, 'cm'))
    })
})
