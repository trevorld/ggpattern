test_that("`scale_pattern_shape()` works", {
  expect_true(is_scale(scale_pattern_shape()))
})

test_that("`scale_pattern_shape()` uses `solid`, `palette`, and theme elements", {
  lvls <- c("a", "b", "c")
  thm <- theme(palette.pattern_shape.discrete = scales::pal_shape(solid = FALSE))
  expect_equal(scale_values(NULL, "pattern_shape", lvls, thm), scales::pal_shape(solid = FALSE)(3))
  expect_equal(scale_values(NULL, "pattern_shape", lvls), scales::pal_shape()(3))
  expect_equal(scale_values(scale_pattern_shape(solid = TRUE), "pattern_shape", lvls, thm),
               scales::pal_shape()(3))
  expect_error(scale_pattern_shape(palette = c(1, 2, 3)), "scales::pal_manual")
  # numeric (`pch`) shapes
  expect_equal(scale_values(scale_pattern_shape(palette = scales::pal_manual(c(1, 2, 3))), "pattern_shape", lvls),
               c(1, 2, 3))
  thm <- theme(palette.pattern_shape.discrete = scales::pal_manual(c(1, 2, 3)))
  expect_equal(scale_values(NULL, "pattern_shape", lvls, thm), c(1, 2, 3))
})
