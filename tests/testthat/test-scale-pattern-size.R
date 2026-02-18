test_that("`scale_pattern_size_continuous()` works", {
  expect_true(is_scale(scale_pattern_size_continuous()))
})

test_that("`scale_pattern_size_continuous()` warns on deprecated `trans`", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(scale_pattern_size_continuous(trans = "identity"),
                 class = "lifecycle_warning_deprecated")
})

test_that("`scale_pattern_size_ordinal()` works", {
  expect_true(is_scale(scale_pattern_size_ordinal()))
})

test_that("`scale_pattern_size_discrete()` works", {
  expect_true(is_scale(scale_pattern_size_discrete()))
})
