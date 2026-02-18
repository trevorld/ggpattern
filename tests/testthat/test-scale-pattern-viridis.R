test_that("viridis discrete scales work", {
  expect_true(is_scale(scale_pattern_colour_viridis_d()))
  expect_true(is_scale(scale_pattern_fill_viridis_d()))
  expect_true(is_scale(scale_pattern_fill2_viridis_d()))
})

test_that("viridis continuous scales work", {
  expect_true(is_scale(scale_pattern_colour_viridis_c()))
  expect_true(is_scale(scale_pattern_fill_viridis_c()))
  expect_true(is_scale(scale_pattern_fill2_viridis_c()))
})
