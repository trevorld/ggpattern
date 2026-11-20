test_that("`scale_pattern_alpha_continuous()` works", {
  expect_true(is_scale(scale_pattern_alpha_continuous()))
})

test_that("`scale_pattern_alpha_ordinal()` works", {
  expect_true(is_scale(scale_pattern_alpha_ordinal()))
})

test_that("`pattern_alpha` scales use `range`, `palette`, and theme elements", {
  thm <- theme(palette.pattern_alpha.continuous = scales::pal_rescale(c(0.5, 0.6)))
  expect_equal(scale_values(NULL, "pattern_alpha", c(0, 1), thm), c(0.5, 0.6))
  expect_equal(scale_values(NULL, "pattern_alpha", c(0, 1)), c(0.1, 1))
  expect_equal(scale_values(scale_pattern_alpha_continuous(range = c(0.2, 0.3)), "pattern_alpha", c(0, 1), thm),
               c(0.2, 0.3))
  expect_equal(scale_values(scale_pattern_alpha_ordinal(palette = function(n) rep(0.5, n)),
                            "pattern_alpha", c("a", "b", "c")),
               rep(0.5, 3))
  expect_error(scale_pattern_alpha_continuous(palette = c(0.2, 0.3)), "must be a palette function")
})
