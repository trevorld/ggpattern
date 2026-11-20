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
