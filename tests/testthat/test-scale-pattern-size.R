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

test_that("`pattern_size` scales use `range`, `palette`, and theme elements", {
  thm <- theme(palette.pattern_size.continuous = scales::pal_area(c(1, 2)))
  expect_equal(scale_values(NULL, "pattern_size", c(0, 1), thm), c(1, 2))
  expect_equal(scale_values(NULL, "pattern_size", c(0, 1)), c(1, 6))
  expect_equal(scale_values(scale_pattern_size_continuous(range = c(2, 3)), "pattern_size", c(0, 1)),
               c(2, 3))
  expect_equal(scale_values(scale_pattern_size_ordinal(range = c(1, 3)), "pattern_size", c("a", "b")),
               c(1, 3))
})
