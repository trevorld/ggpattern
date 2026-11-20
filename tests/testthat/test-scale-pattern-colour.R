test_that("colour/fill continuous scales work for gradient type", {
  expect_true(is_scale(scale_pattern_colour_continuous(type = "gradient")))
  expect_true(is_scale(scale_pattern_fill_continuous(type = "gradient")))
  expect_true(is_scale(scale_pattern_fill2_continuous(type = "gradient")))
})

test_that("colour/fill continuous scales work for viridis type", {
  expect_true(is_scale(scale_pattern_colour_continuous(type = "viridis")))
  expect_true(is_scale(scale_pattern_fill_continuous(type = "viridis")))
  expect_true(is_scale(scale_pattern_fill2_continuous(type = "viridis")))
})

test_that("colour/fill continuous scales error on unknown type", {
  expect_error(scale_pattern_colour_continuous(type = "nonsense"), "Unknown scale type")
  expect_error(scale_pattern_fill_continuous(type = "nonsense"), "Unknown scale type")
  expect_error(scale_pattern_fill2_continuous(type = "nonsense"), "Unknown scale type")
})

test_that("pattern colour scales use their own theme elements", {
  lvls <- c("a", "b", "c")
  # `palette.fill.*` and `palette.colour.*` aren't used
  thm <- theme(palette.fill.discrete = c("red", "green", "blue"),
               palette.colour.continuous = c("red", "blue"))
  expect_equal(scale_values(NULL, "pattern_fill", lvls, thm), scales::pal_hue()(3))
  expect_equal(scale_values(NULL, "pattern_colour", c(0, 1), thm), c("#132B43", "#56B1F7"))

  thm <- theme(palette.pattern_fill.discrete = c("black", "grey50", "white"),
               palette.pattern_fill2.discrete = c("red", "green", "blue"),
               palette.pattern_colour.continuous = c("red", "blue"))
  expect_equal(scale_values(NULL, "pattern_fill", lvls, thm), c("black", "grey50", "white"))
  expect_equal(scale_values(NULL, "pattern_fill2", lvls, thm), c("red", "green", "blue"))
  expect_equal(scale_values(NULL, "pattern_colour", c(0, 1), thm), c("#FF0000", "#0000FF"))

  # `palette` argument
  expect_equal(scale_values(scale_pattern_colour_discrete(palette = c("red", "blue")), "pattern_colour", c("a", "b")),
               c("red", "blue"))
  expect_equal(scale_values(scale_pattern_fill_continuous(palette = c("red", "blue")), "pattern_fill", c(0, 1)),
               c("#FF0000", "#0000FF"))
  # `scale_pattern_*_hue()` and `scale_pattern_*_gradient()` arguments and the `type` option
  expect_equal(scale_values(scale_pattern_fill2_discrete(l = 30), "pattern_fill2", lvls, thm),
               scales::pal_hue(l = 30)(3))
  expect_equal(scale_values(scale_pattern_fill2_continuous(low = "white", high = "black"),
                            "pattern_fill2", c(0, 1), thm),
               c("#FFFFFF", "#000000"))
  withr::local_options(ggplot2.continuous.colour = "viridis")
  expect_equal(scale_values(scale_pattern_colour_continuous(), "pattern_colour", c(0, 1), thm),
               c("#440154", "#FDE725"))
})

test_that("continuous colour scales keep their colourbar with `aesthetics = \"pattern_color\"`", {
  df <- data.frame(x = 1:2, y = 1, z = c(0, 1))
  p <- ggplot(df, aes(x, y, pattern_colour = z)) +
    geom_col_pattern() +
    scale_pattern_colour_continuous(aesthetics = "pattern_color")
  expect_no_warning(ggplot_gtable(ggplot_build(p)))
})
