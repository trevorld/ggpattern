context("array")
test_that("array patterns work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    df <- data.frame(trt = c("a", "b", "c"),
                     outcome = c(2.3, 1.9, 3.2))

    pattern_ggplot <- function(pattern) {
        ggplot(df, aes(trt, outcome)) +
            geom_col_pattern(aes(fill=trt),
                             colour='black',
                             pattern = pattern) +
            theme_bw() +
            labs(title = pattern)
    }

    expect_doppelganger("gradient", pattern_ggplot("gradient"))
    expect_doppelganger("magick", pattern_ggplot("magick"))
    expect_doppelganger("placeholder", pattern_ggplot("placeholder"))
    # plasma pattern is random
    # expect_doppelganger("plasma", pattern_ggplot("image"))
})

test_that("image pattern work as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    logo_filename <- system.file("img", "Rlogo.png" , package="png")
    magpie_filename <- system.file("img", "magpie.jpg", package="ggpattern")
    bug_filename    <- system.file("img", "bug.jpg"   , package="ggpattern")

    df1 <- tibble(
      trt      = c("a", "b", "c"),
      outcome  = c(2.3, 1.9, 3.2),
      gravity  = c('South', 'North', 'West'),
      filltype = c('squish', 'fit' , 'expand'),
      scale    = c(1, 2, 0.5),
      filename = c(logo_filename, magpie_filename, bug_filename)
    )

    expect_doppelganger("image_logo_none", {
        ggplot(df1, aes(trt, outcome)) +
          geom_col_pattern(
            aes(
              fill            = trt,
              pattern_gravity = I(gravity),
              pattern_scale   = I(scale)
            ),
            pattern          = 'image',
            pattern_filename = logo_filename,
            pattern_type     = 'none',
            colour           = 'black'
          ) +
          theme_bw(15) +
          labs(
            title    = "ggpattern::geom_col_pattern()",
            subtitle = "pattern = 'image', pattern_type = 'none'"
          ) +
          theme(legend.key.size = unit(1.5, 'cm')) +
          coord_fixed(ratio = 1/2)
    })

    expect_doppelganger("image_logo_variety", {
                            ggplot(df1, aes(trt, outcome)) +
        geom_col_pattern(
           aes(
             fill            = trt,
             pattern_gravity = I(gravity),
             pattern_type    = I(filltype)
           ),
           pattern          = 'image',
           colour           = 'black',
           pattern_filename = logo_filename,
        ) +
        theme_bw(15) +
        labs(
          title    = "ggpattern::geom_col_pattern()",
          subtitle = "pattern = 'image'"
        ) +
        # theme(legend.key.size = unit(1.5, 'cm')) +
        theme(legend.position = 'none') +
        coord_fixed(ratio = 1/2)
    })
})
