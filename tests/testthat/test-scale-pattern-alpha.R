test_that("`scale_pattern_alpha_continuous()` works", {
  expect_true(is_scale(scale_pattern_alpha_continuous()))
})

test_that("`scale_pattern_alpha_ordinal()` works", {
  expect_true(is_scale(scale_pattern_alpha_ordinal()))
})
