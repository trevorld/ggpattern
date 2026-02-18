test_that("hue scales work", {
  expect_true(is_scale(scale_pattern_colour_hue()))
  expect_true(is_scale(scale_pattern_fill_hue()))
  expect_true(is_scale(scale_pattern_fill2_hue()))
})
