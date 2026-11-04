test_that("`scale_pattern_linewidth_continuous()` works", {
  expect_true(is_scale(scale_pattern_linewidth_continuous()))
  expect_true(is_scale(scale_pattern_linewidth()))
})

test_that("`scale_pattern_linewidth_continuous()` warns on deprecated `trans`", {
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(scale_pattern_linewidth_continuous(trans = "identity"),
                 class = "lifecycle_warning_deprecated")
})

test_that("`scale_pattern_linewidth_ordinal()` works", {
  expect_true(is_scale(scale_pattern_linewidth_ordinal()))
})

test_that("`scale_pattern_linewidth_discrete()` works", {
  expect_true(is_scale(scale_pattern_linewidth_discrete()))
})

test_that("`pattern_linewidth` scales linearly like `ggplot2::scale_linewidth()`", {
  sc <- scale_pattern_linewidth_continuous(range = c(1, 6))
  expect_equal(sc$palette(c(0, 0.5, 1)), c(1, 3.5, 6))
  sc <- scale_pattern_linewidth_ordinal(range = c(2, 6))
  expect_equal(sc$palette(3), c(2, 4, 6))
})
