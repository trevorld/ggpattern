test_that("auto-generated continuous scales work", {
  # Scales with default `range`
  simple <- c(
    "scale_pattern_angle_continuous",
    "scale_pattern_density_continuous",
    "scale_pattern_spacing_continuous",
    "scale_pattern_xoffset_continuous",
    "scale_pattern_yoffset_continuous",
    "scale_pattern_aspect_ratio_continuous",
    "scale_pattern_key_scale_factor_continuous",
    "scale_pattern_scale_continuous",
    "scale_pattern_rot_continuous"
  )
  for (fn in simple) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
  }

  # Scales that require `range`
  with_range <- c(
    "scale_pattern_phase_continuous",
    "scale_pattern_frequency_continuous",
    "scale_pattern_res_continuous"
  )
  for (fn in with_range) {
    x <- do.call(fn, list(range = c(0, 1)))
    expect_true(is_scale(x), label = fn)
  }

  # Scales that require `choices`
  with_choices <- c(
    "scale_pattern_type_continuous",
    "scale_pattern_subtype_continuous",
    "scale_pattern_filename_continuous",
    "scale_pattern_filter_continuous",
    "scale_pattern_gravity_continuous",
    "scale_pattern_orientation_continuous",
    "scale_pattern_grid_continuous",
    "scale_pattern_units_continuous"
  )
  for (fn in with_choices) {
    x <- do.call(fn, list(choices = c("a", "b")))
    expect_true(is_scale(x), label = fn)
  }
})

test_that("auto-generated discrete scales work", {
  # Scales with default `range`
  simple <- c(
    "scale_pattern_angle_discrete",
    "scale_pattern_density_discrete",
    "scale_pattern_spacing_discrete",
    "scale_pattern_xoffset_discrete",
    "scale_pattern_yoffset_discrete",
    "scale_pattern_aspect_ratio_discrete",
    "scale_pattern_key_scale_factor_discrete",
    "scale_pattern_scale_discrete",
    "scale_pattern_rot_discrete"
  )
  for (fn in simple) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
  }

  # Scales that require `range`
  with_range <- c(
    "scale_pattern_phase_discrete",
    "scale_pattern_frequency_discrete",
    "scale_pattern_res_discrete"
  )
  for (fn in with_range) {
    x <- do.call(fn, list(range = c(0, 1)))
    expect_true(is_scale(x), label = fn)
  }

  # Scales that require `choices`
  with_choices <- c(
    "scale_pattern_type_discrete",
    "scale_pattern_subtype_discrete",
    "scale_pattern_filename_discrete"
  )
  for (fn in with_choices) {
    x <- do.call(fn, list(choices = c("a", "b")))
    expect_true(is_scale(x), label = fn)
  }

  # Scales with default `choices`
  with_default_choices <- c(
    "scale_pattern_filter_discrete",
    "scale_pattern_gravity_discrete",
    "scale_pattern_orientation_discrete",
    "scale_pattern_grid_discrete",
    "scale_pattern_units_discrete"
  )
  for (fn in with_default_choices) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
  }
})

test_that("discrete identity scales work", {
  fns <- c(
    "scale_pattern_type_identity",
    "scale_pattern_subtype_identity",
    "scale_pattern_linetype_identity",
    "scale_pattern_colour_identity",
    "scale_pattern_fill_identity",
    "scale_pattern_fill2_identity",
    "scale_pattern_filename_identity",
    "scale_pattern_filter_identity",
    "scale_pattern_gravity_identity",
    "scale_pattern_orientation_identity",
    "scale_pattern_grid_identity",
    "scale_pattern_units_identity"
  )
  for (fn in fns) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
  }
})

test_that("auto-generated manual scales work", {
  fns <- c(
    "scale_pattern_type_manual",
    "scale_pattern_subtype_manual",
    "scale_pattern_angle_manual",
    "scale_pattern_density_manual",
    "scale_pattern_spacing_manual",
    "scale_pattern_xoffset_manual",
    "scale_pattern_yoffset_manual",
    "scale_pattern_alpha_manual",
    "scale_pattern_linetype_manual",
    "scale_pattern_linewidth_manual",
    "scale_pattern_size_manual",
    "scale_pattern_shape_manual",
    "scale_pattern_colour_manual",
    "scale_pattern_fill_manual",
    "scale_pattern_fill2_manual",
    "scale_pattern_aspect_ratio_manual",
    "scale_pattern_key_scale_factor_manual",
    "scale_pattern_filename_manual",
    "scale_pattern_filter_manual",
    "scale_pattern_gravity_manual",
    "scale_pattern_scale_manual",
    "scale_pattern_orientation_manual",
    "scale_pattern_phase_manual",
    "scale_pattern_frequency_manual",
    "scale_pattern_grid_manual",
    "scale_pattern_res_manual",
    "scale_pattern_rot_manual",
    "scale_pattern_units_manual"
  )
  for (fn in fns) {
    x <- do.call(fn, list(values = c(a = 1, b = 2)))
    expect_true(is_scale(x), label = fn)
  }
})

test_that("scales without a default palette need one when used", {
  fns <- c(
    scale_pattern_type_continuous = "pattern_type",
    scale_pattern_subtype_continuous = "pattern_subtype",
    scale_pattern_filename_continuous = "pattern_filename",
    scale_pattern_phase_continuous = "pattern_phase",
    scale_pattern_frequency_continuous = "pattern_frequency",
    scale_pattern_res_continuous = "pattern_res",
    scale_pattern_type_discrete = "pattern_type",
    scale_pattern_subtype_discrete = "pattern_subtype",
    scale_pattern_filename_discrete = "pattern_filename",
    scale_pattern_phase_discrete = "pattern_phase",
    scale_pattern_frequency_discrete = "pattern_frequency",
    scale_pattern_res_discrete = "pattern_res"
  )
  for (fn in names(fns)) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
    values <- if (endsWith(fn, "_discrete")) c("a", "b") else c(1, 2)
    expect_error(scale_values(x, fns[[fn]], values), "needs a palette", label = fn)
  }
})

test_that("auto-generated continuous scales warn on deprecated `trans`", {
  rlang::local_options(lifecycle_verbosity = "warning")

  # Scales with default `range`
  simple <- c(
    "scale_pattern_angle_continuous",
    "scale_pattern_density_continuous",
    "scale_pattern_spacing_continuous",
    "scale_pattern_xoffset_continuous",
    "scale_pattern_yoffset_continuous",
    "scale_pattern_aspect_ratio_continuous",
    "scale_pattern_key_scale_factor_continuous",
    "scale_pattern_scale_continuous",
    "scale_pattern_rot_continuous"
  )
  for (fn in simple) {
    expect_warning(do.call(fn, list(trans = "identity")),
                   class = "lifecycle_warning_deprecated", label = fn)
  }

  # Scales that require `range`
  with_range <- c(
    "scale_pattern_phase_continuous",
    "scale_pattern_frequency_continuous",
    "scale_pattern_res_continuous"
  )
  for (fn in with_range) {
    expect_warning(do.call(fn, list(range = c(0, 1), trans = "identity")),
                   class = "lifecycle_warning_deprecated", label = fn)
  }

  # Scales that require `choices`
  with_choices <- c(
    "scale_pattern_type_continuous",
    "scale_pattern_subtype_continuous",
    "scale_pattern_filename_continuous",
    "scale_pattern_filter_continuous",
    "scale_pattern_gravity_continuous",
    "scale_pattern_orientation_continuous",
    "scale_pattern_grid_continuous",
    "scale_pattern_units_continuous"
  )
  for (fn in with_choices) {
    expect_warning(do.call(fn, list(choices = c("a", "b"), trans = "identity")),
                   class = "lifecycle_warning_deprecated", label = fn)
  }
})

test_that("scales with a default palette use it when `choices` or `range` is NULL", {
  expect_equal(scale_values(scale_pattern_angle_continuous(range = NULL), "pattern_angle", c(0, 1)),
               c(0, 90))
  expect_equal(scale_values(scale_pattern_angle_discrete(range = NULL), "pattern_angle", c("a", "b")),
               c(0, 90))
  expect_equal(scale_values(scale_pattern_grid_continuous(choices = NULL), "pattern_grid", c(0, 1)),
               c("square", "hex"))
  expect_equal(scale_values(scale_pattern_grid_discrete(choices = NULL), "pattern_grid", c("a", "b")),
               c("square", "hex"))
})

test_that("continuous identity scales work", {
  fns <- c(
    "scale_pattern_angle_identity",
    "scale_pattern_density_identity",
    "scale_pattern_spacing_identity",
    "scale_pattern_xoffset_identity",
    "scale_pattern_yoffset_identity",
    "scale_pattern_alpha_identity",
    "scale_pattern_linewidth_identity",
    "scale_pattern_size_identity",
    "scale_pattern_shape_identity",
    "scale_pattern_aspect_ratio_identity",
    "scale_pattern_key_scale_factor_identity",
    "scale_pattern_scale_identity",
    "scale_pattern_phase_identity",
    "scale_pattern_frequency_identity",
    "scale_pattern_res_identity",
    "scale_pattern_rot_identity"
  )
  for (fn in fns) {
    x <- do.call(fn, list())
    expect_true(is_scale(x), label = fn)
  }
})

test_that("generated scales take a `palette` argument (#172)", {
  x <- c("hexagonal", "rhombille", "pythagorean")
  lvls <- c("a", "b", "c")
  expect_equal(scale_values(scale_pattern_type_discrete(palette = scales::as_discrete_pal(x)),
                            "pattern_type", lvls),
               x)
  expect_equal(scale_values(scale_pattern_type_discrete(palette = x), "pattern_type", lvls), x)
  # `palette` takes precedence over `choices`
  expect_equal(scale_values(scale_pattern_type_discrete(choices = x, palette = scales::pal_manual(rev(x))),
                            "pattern_type", lvls),
               rev(x))
  expect_equal(scale_values(scale_pattern_angle_continuous(range = c(0, 90),
                                                           palette = scales::pal_rescale(c(10, 20))),
                            "pattern_angle", c(0, 1)),
               c(10, 20))
  expect_equal(scale_values(scale_pattern_angle_discrete(palette = function(n) seq_len(n)),
                            "pattern_angle", lvls),
               1:3)
  # vectors of values belong in `choices` or `range`
  expect_error(scale_pattern_type_continuous(palette = x), "the `choices` argument")
  expect_error(scale_pattern_angle_discrete(palette = c(0, 45, 90)), "the `range` argument")
  expect_error(scale_pattern_angle_continuous(palette = c(0, 45)), "scale_pattern_angle_manual")
})

test_that("generated scales use `palette.<aes>.<type>` theme elements", {
  x <- c("hexagonal", "rhombille", "pythagorean")
  lvls <- c("a", "b", "c")
  thm <- theme(palette.pattern_type.discrete = x,
               palette.pattern_angle.continuous = scales::pal_rescale(c(0, 45)),
               palette.pattern_density.discrete = function(n) seq(0.1, 0.3, length.out = n))
  # default scales
  expect_equal(scale_values(NULL, "pattern_type", lvls, thm), x)
  expect_equal(scale_values(NULL, "pattern_angle", c(0, 1), thm), c(0, 45))
  expect_equal(scale_values(NULL, "pattern_density", lvls, thm), c(0.1, 0.2, 0.3))
  # explicit scales without `choices`, `range`, or `palette`
  expect_equal(scale_values(scale_pattern_type_discrete(), "pattern_type", lvls, thm), x)
  expect_equal(scale_values(scale_pattern_angle_continuous(), "pattern_angle", c(0, 1), thm), c(0, 45))
  # `choices`, `range`, and `palette` take precedence over the theme
  expect_equal(scale_values(scale_pattern_type_discrete(choices = rev(x)), "pattern_type", lvls, thm),
               rev(x))
  expect_equal(scale_values(scale_pattern_angle_continuous(range = c(0, 90)), "pattern_angle", c(0, 1), thm),
               c(0, 90))
  expect_equal(scale_values(scale_pattern_type_discrete(palette = rev(x)), "pattern_type", lvls, thm),
               rev(x))
  # `theme_set()`
  old <- theme_set(theme_bw() + thm)
  on.exit(theme_set(old), add = TRUE)
  expect_equal(scale_values(NULL, "pattern_type", lvls), x)
  expect_equal(scale_values(NULL, "pattern_angle", c(0, 1)), c(0, 45))
})
