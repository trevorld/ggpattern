test_that("`palette.<aes>.<type>` theme elements are registered", {
  tree <- ggplot2::get_element_tree()
  for (aes in c("pattern", "pattern_type", "pattern_angle", "pattern_fill", "pattern_units")) {
    for (type in c("discrete", "continuous")) {
      expect_true(paste0("palette.", aes, ".", type) %in% names(tree), label = aes)
    }
  }
  expect_false("palette.pattern_option_1.discrete" %in% names(tree))
  # only the classes that can be converted into a palette
  expect_equal(tree$palette.pattern_fill.continuous$class, c("character", "function"))
  expect_equal(tree$palette.pattern_type.discrete$class, c("character", "function"))
  expect_equal(tree$palette.pattern_type.continuous$class, "function")
  expect_equal(tree$palette.pattern_angle.discrete$class, "function")
})

test_that("theme palette elements must be convertible into a palette", {
  lvls <- c("a", "b", "c")
  expect_error(scale_values(NULL, "pattern_angle", lvls, theme(palette.pattern_angle.discrete = c(0, 45, 90))),
               "must be a <function>")
  expect_error(scale_values(NULL, "pattern_type", c(0, 1), theme(palette.pattern_type.continuous = c("a", "b"))),
               "must be a <function>")
})
