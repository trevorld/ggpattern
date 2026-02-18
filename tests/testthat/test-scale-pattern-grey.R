test_that("grey scales work", {
  expect_true(is_scale(scale_pattern_colour_grey()))
  expect_true(is_scale(scale_pattern_fill_grey()))
  expect_true(is_scale(scale_pattern_fill2_grey()))
})
