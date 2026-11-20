test_that("`scale_pattern_linewidth_continuous()` works", {
  expect_true(is_scale(scale_pattern_linewidth_continuous()))
  expect_true(is_scale(scale_pattern_linewidth()))
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

test_that("`pattern_linewidth` scales use `range`, `palette`, and theme elements", {
  lvls <- c("a", "b", "c")
  thm <- theme(palette.pattern_linewidth.discrete = function(n) seq_len(n))
  expect_equal(scale_values(NULL, "pattern_linewidth", lvls, thm), 1:3)
  expect_equal(scale_values(NULL, "pattern_linewidth", c(0, 1)), c(1, 6))
  expect_equal(scale_values(scale_pattern_linewidth_continuous(palette = scales::pal_rescale(c(2, 3))),
                            "pattern_linewidth", c(0, 1)),
               c(2, 3))
  expect_equal(scale_values(scale_pattern_linewidth_ordinal(range = c(1, 3)), "pattern_linewidth", lvls),
               1:3)
})
