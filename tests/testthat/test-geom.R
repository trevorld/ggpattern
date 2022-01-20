context("geom")
test_that("geometry patterns work as expected", {
    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    rlang::local_options(ggpattern_use_R4.1_masks = FALSE)

    expect_doppelganger("bar", {
        ggplot(ggplot2::mpg, aes(class)) +
          geom_bar_pattern(
            aes(
              pattern = class,
              pattern_angle = class
            ),
            fill            = 'white',
            colour          = 'black',
            pattern_spacing = 0.025
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_bar_pattern()") +
          theme(legend.position = 'none') +
          coord_fixed(ratio = 1/15) +
          scale_pattern_discrete(guide = guide_legend(nrow = 1))
    })

    suppressWarnings( # Removed 478 rows containing non-finite values (stat_bin2d)
    expect_doppelganger("bin2d", {
        ggplot(ggplot2::diamonds, aes(x, y)) +
            xlim(4, 10) + ylim(4, 10) +
            geom_bin2d_pattern(aes(pattern_spacing = ..density..),
                               fill = 'white', bins = 6, colour = 'black', size = 1) +
            theme_bw(18) +
            theme(legend.position = 'none') +
            labs(title = "ggpattern::geom_bin2d_pattern()")
    }))

    expect_doppelganger("boxplot", {
        ggplot(ggplot2::mpg, aes(class, hwy)) +
            geom_boxplot_pattern(
                 aes(
                     pattern      = class,
                     pattern_fill = class
                     ),
                 pattern_spacing = 0.03
                 ) +
            theme_bw(18) +
            labs(title = "ggpattern::geom_boxplot_pattern()") +
            theme(legend.position = 'none') +
            coord_fixed(1/8)
    })

    expect_doppelganger("col", {
        df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
        ggplot(df, aes(trt, outcome)) +
          geom_col_pattern(
            aes(
              pattern = trt,
              fill    = trt
            ),
            colour                   = 'black',
            pattern_density          = 0.5,
            pattern_key_scale_factor = 1.11
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_col_pattern()") +
          theme(legend.position = 'none') +
          coord_fixed(ratio = 1/2)
    })

    expect_doppelganger("crossbar", {
        df <- data.frame(
             trt = factor(c(1, 1, 2, 2)),
             resp = c(1, 5, 3, 4),
             group = factor(c(1, 2, 1, 2)),
             upper = c(1.1, 5.3, 3.3, 4.2),
             lower = c(0.8, 4.6, 2.4, 3.6)
        )

        ggplot(df, aes(trt, resp, colour = group)) +
            geom_crossbar_pattern(
              aes(
                  ymin          = lower,
                  ymax          = upper,
                  pattern_angle = trt,
                  pattern       = group
                  ), width        = 0.2,
              pattern_spacing = 0.02
              ) +
            theme_bw(18) +
            labs(title = "ggpattern::geom_crossbar_pattern()") +
            theme(legend.position = 'none') +
            coord_fixed(ratio = 1/3)
    })

    expect_doppelganger("density", {
        ggplot(mtcars) +
        geom_density_pattern(
          aes(
            x            = mpg,
            pattern_fill = as.factor(cyl),
            pattern      = as.factor(cyl)
          ),
          fill                     = 'white',
          pattern_key_scale_factor = 1.2,
          pattern_density          = 0.4
        ) +
        theme_bw(18) +
        labs(title = "ggpattern::geom_density_pattern()") +
        theme(legend.key.size = unit(2, 'cm')) +
        coord_fixed(ratio = 100)
    })

    expect_doppelganger("polygon", {
      angle <- seq(0, 2*pi, length.out = 7) + pi/6
      polygon_df <- data.frame(
        angle = angle,
        x     = cos(angle),
        y     = sin(angle)
      )

      ggplot(polygon_df) +
        geom_polygon_pattern(
          aes(x = x, y = y),
          fill            = 'white',
          colour          = 'black',
          pattern_spacing = 0.15,
          pattern_density = 0.4,
          pattern_fill    = 'lightblue',
          pattern_colour  = '#002366',
          pattern_angle   = 45
        ) +
        labs(title = "ggpattern") +
        coord_equal() +
        theme_bw(25) +
        theme(axis.title = element_blank())
    })

    expect_doppelganger("polygon_hole", {
      # example modified from {ggplot2} unit tests
      tbl <- data.frame(
        x = c(
          0, 10, 10, 0,
          20, 30, 30, 20,
          22, 28, 28, 22
        ),
        y = c(
          0, 0, 10, 10,
          20, 20, 30, 30,
          22, 22, 28, 28
        ),
        group = c(rep(1, 4), rep(2, 8)),
        subgroup = c(rep(1, 8), rep(2, 4))
      )

      p <- ggplot(tbl, aes(x, y, group = group, subgroup = subgroup)) +
        geom_polygon_pattern(fill="lightblue", color="black")
    })

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

    suppressWarnings( # outline.type = "legacy" is only for backward-compatibility...
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
              outline.type    = 'legacy'
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_ribbon_pattern()")
    }))

    expect_doppelganger("tile", {
        df <- data.frame(
          x = rep(c(2, 5, 7, 9, 12), 2),
          y = rep(c(1, 2), each = 5),
          z = factor(rep(1:5, each = 2)),
          w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
        )

        ggplot(df, aes(x, y)) +
          geom_tile_pattern(
            aes(
              fill    = z,
              pattern = z
            ),
            colour = "grey50"
          ) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_tile_pattern()") +
          theme(
            legend.position = 'bottom',
            legend.key.size = unit(1.5, 'cm')
          ) +
          coord_fixed(ratio = 4)
    })

    expect_doppelganger("violin", {
        ggplot(mtcars, aes(as.factor(cyl), mpg)) +
          geom_violin_pattern(aes(pattern = as.factor(cyl))) +
          theme_bw(18) +
          labs(title = "ggpattern::geom_violin_pattern()") +
          theme(
            legend.key.size  = unit(2, 'cm')
          ) +
          coord_fixed(1/15)
    })
})

test_that("geom_sf_pattern() works as expected", {
    skip_on_ci()
    skip_if_not_installed("sf")
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    rlang::local_options(ggpattern_use_R4.1_masks = FALSE)

    expect_doppelganger("sf", {
      nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
      indices <- which(nc$CNTY_ID >= 1820 & nc$CNTY_ID <= 1830)
      nc <- nc[indices, ]

      p <- ggplot(nc) +
        geom_sf_pattern(
          aes(
            pattern = NAME,
            fill    = NAME
          ),
          pattern_aspect_ratio = 2.8
        ) +
        theme_bw(15) +
        theme(legend.key.size = unit(1.5, 'cm')) +
        labs(title = "ggpattern::geom_sf()")

      p
    })

    expect_doppelganger("sf-hole", {
      # coords <- read.csv(textConnection(
      # 'x,y,idx
      # 0,0,1
      # 4,0,1
      # 4,4,1
      # 0,4,1
      # 0,0,1
      # 1,1,2
      # 2,1,2
      # 2,2,2
      # 1,2,2
      # 1,1,2'))

      # this_sf <- coords %>%
      #   sfheaders::sf_polygon(x='x', y='y', linestring_id = 'idx')
      # Avoid {sfheaders} Suggests by using output from `dput(this_sf)`
      this_sf <- structure(list(id = 1L,
            geometry = structure(list(structure(list(
                                                     structure(c(0, 4, 4, 0, 0, 0, 0, 4, 4, 0), .Dim = c(5L, 2L)),
                                                     structure(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), .Dim = c(5L, 2L))),
                                                class = c("XY", "POLYGON", "sfg"))),
                                 n_empty = 0L,
                                 crs = structure(list(
                                                      input = NA_character_,
                                                      wkt = NA_character_),
                                                 class = "crs"),
                                 class = c("sfc_POLYGON", "sfc"),
                                 precision = 0,
                                 bbox = structure(c(xmin = 0, ymin = 0, xmax = 4, ymax = 4),
                                                  class = "bbox"))),
                           class = c("sf", "data.frame"),
                           sf_column = "geometry", row.names = 1L)

      ggplot(this_sf) +
        geom_sf_pattern(aes(geometry = geometry), fill = 'lightblue')
    })
})

test_that("geom_map_pattern() works as expected", {
    skip_on_ci()
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    skip_if_not_installed("maps")
    expect_doppelganger("map", {
      library(maps)

      crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)

      states_map <- map_data("state")
      ggplot(crimes, aes(map_id = state)) +
        geom_map_pattern(
          aes(
            # fill            = Murder,
            pattern_spacing = state,
            pattern_density = state,
            pattern_angle   = state,
            pattern         = state
          ),
          fill = 'white',
          map = states_map
        ) +
        expand_limits(x = states_map$long, y = states_map$lat) +
        coord_map() +
        theme_bw() +
        labs(title = "ggpattern::geom_map_pattern()")
    })
})
