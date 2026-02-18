test_that("`scale_pattern_continuous()` works", {
  expect_true(is_scale(scale_pattern_continuous()))
})

test_that("`scale_pattern_continuous()` warns on deprecated `trans`", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(scale_pattern_continuous(trans = "identity"),
                 class = "lifecycle_warning_deprecated")
})

test_that("`scale_pattern_discrete()` works", {
  expect_true(is_scale(scale_pattern_discrete()))
})

test_that("`scale_pattern_discrete()` errors when `choices` is NULL", {
  expect_error(scale_pattern_discrete(choices = NULL),
               'must specify "choices" argument')
})

test_that("`scale_pattern_manual()` works", {
  expect_true(is_scale(scale_pattern_manual(values = c(a = "stripe", b = "crosshatch"))))
})

test_that("`scale_pattern_identity()` works", {
  expect_true(is_scale(scale_pattern_identity()))
})
