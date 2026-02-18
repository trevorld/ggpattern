test_that("gradient scales work", {
  expect_true(is_scale(scale_pattern_colour_gradient()))
  expect_true(is_scale(scale_pattern_fill_gradient()))
  expect_true(is_scale(scale_pattern_fill2_gradient()))
})

test_that("gradient2 scales work", {
  expect_true(is_scale(scale_pattern_colour_gradient2()))
  expect_true(is_scale(scale_pattern_fill_gradient2()))
  expect_true(is_scale(scale_pattern_fill2_gradient2()))
})

test_that("gradientn scales work", {
  expect_true(is_scale(scale_pattern_colour_gradientn(colours = c("red", "blue"))))
  expect_true(is_scale(scale_pattern_fill_gradientn(colours = c("red", "blue"))))
  expect_true(is_scale(scale_pattern_fill2_gradientn(colours = c("red", "blue"))))
})
