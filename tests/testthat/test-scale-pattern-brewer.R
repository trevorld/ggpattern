test_that("brewer scales work", {
  expect_true(is_scale(scale_pattern_colour_brewer()))
  expect_true(is_scale(scale_pattern_fill_brewer()))
  expect_true(is_scale(scale_pattern_fill2_brewer()))
})

test_that("distiller scales work", {
  expect_true(is_scale(scale_pattern_colour_distiller()))
  expect_true(is_scale(scale_pattern_fill_distiller()))
  expect_true(is_scale(scale_pattern_fill2_distiller()))
})
