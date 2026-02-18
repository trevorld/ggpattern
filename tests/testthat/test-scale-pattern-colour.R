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
