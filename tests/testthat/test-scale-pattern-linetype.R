test_that("`scale_pattern_linetype()` works", {
  expect_true(is_scale(scale_pattern_linetype()))
})

test_that("`scale_pattern_linetype()` uses `palette` and theme elements", {
  lvls <- c("a", "b", "c")
  thm <- theme(palette.pattern_linetype.discrete = c("dotted", "dashed", "solid"))
  expect_equal(scale_values(NULL, "pattern_linetype", lvls, thm), c("dotted", "dashed", "solid"))
  expect_equal(scale_values(NULL, "pattern_linetype", lvls), scales::pal_linetype()(3))
  expect_equal(scale_values(scale_pattern_linetype(palette = c("dotted", "solid")), "pattern_linetype", c("a", "b")),
               c("dotted", "solid"))
  expect_error(scale_pattern_linetype(palette = 1:3), "scale_pattern_linetype_manual")
})
