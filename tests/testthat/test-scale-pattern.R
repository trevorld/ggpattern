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

test_that("`scale_pattern_discrete()` and `scale_pattern_continuous()` take `choices` and `palette`", {
  expect_equal(scale_values(scale_pattern_discrete(choices = NULL), "pattern", c("a", "b", "c")),
               c("stripe", "crosshatch", "circle"))
  expect_equal(scale_values(scale_pattern_discrete(choices = c("circle", "none")), "pattern", c("a", "b")),
               c("circle", "none"))
  expect_equal(scale_values(scale_pattern_discrete(palette = c("none", "circle")), "pattern", c("a", "b")),
               c("none", "circle"))
  expect_equal(scale_values(scale_pattern_continuous(choices = NULL), "pattern", c(0, 1)),
               c("stripe", "circle"))
  expect_equal(scale_values(scale_pattern_continuous(palette = function(x) ifelse(x > 0.5, "circle", "none")),
                            "pattern", c(0, 1)),
               c("none", "circle"))
})

test_that("continuous `choices` scales split the data into equal-width bins (#176)", {
  choices <- c("stripe", "crosshatch", "circle")
  expect_equal(scale_values(scale_pattern_continuous(choices = choices), "pattern", 1:10),
               rep(choices, c(4, 3, 3)))
  # Same choices as the discrete scale gives the values as a factor
  expect_equal(scale_values(scale_pattern_continuous(choices = choices), "pattern", 1:10),
               scale_values(scale_pattern_discrete(choices = choices), "pattern", factor(1:10)))
  # Including values that are only on a bin edge up to rounding error (e.g. 0.6 * 5)
  choices <- as.character(1:5)
  x <- seq(0, 1, by = 0.1)
  expect_equal(scale_values(scale_pattern_continuous(choices = choices), "pattern", x),
               scale_values(scale_pattern_discrete(choices = choices), "pattern", factor(x)))
  expect_equal(scale_values(scale_pattern_type_continuous(choices = c("a", "b")), "pattern_type",
                            c(0, 0.5, 0.51, 1, NA)),
               c("a", "a", "b", "b", NA))
  # Out of bounds values are `NA` (like `{ggplot2}`'s binned scales)
  expect_equal(pal_choices_continuous(c("a", "b"))(c(-0.1, 0, 1, 1.1)), c(NA, "a", "b", NA))
})

test_that("`scale_pattern_manual()` works", {
  expect_true(is_scale(scale_pattern_manual(values = c(a = "stripe", b = "crosshatch"))))
})

test_that("`scale_pattern_identity()` works", {
  expect_true(is_scale(scale_pattern_identity()))
})

test_that("`scale_pattern_discrete()` uses the `palette.pattern.discrete` theme element", {
  lvls <- c("a", "b", "c")
  thm <- theme(palette.pattern.discrete = c("circle", "none", "stripe"))
  expect_equal(scale_values(NULL, "pattern", lvls, thm), c("circle", "none", "stripe"))
  expect_equal(scale_values(NULL, "pattern", lvls), c("stripe", "crosshatch", "circle"))
})
