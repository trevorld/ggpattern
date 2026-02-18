test_that("`geom_area_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("area", {
        huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
        ggplot(huron, aes(year)) +
            geom_area_pattern(aes(y = level))
    })
})

test_that("`geom_ribbon_pattern()` works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("ribbon", {
        huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))

        ggplot(huron, aes(year)) +
          geom_ribbon_pattern(
              aes(
                ymin = level - 1,
                ymax = level + 1
              ),
              fill            = NA,
              colour          = 'black',
              pattern         = 'circle',
              pattern_spacing = 0.03,
              pattern_density = 0.5,
              pattern_angle   = 30,
              outline.type    = 'full'
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_ribbon_pattern()")
    })
})
