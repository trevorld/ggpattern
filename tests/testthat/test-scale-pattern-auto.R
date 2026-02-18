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

test_that("continuous scales error when `choices` is NULL", {
  fns <- c(
    "scale_pattern_type_continuous",
    "scale_pattern_subtype_continuous",
    "scale_pattern_filename_continuous",
    "scale_pattern_filter_continuous",
    "scale_pattern_gravity_continuous",
    "scale_pattern_orientation_continuous",
    "scale_pattern_grid_continuous",
    "scale_pattern_units_continuous"
  )
  for (fn in fns) {
    expect_error(do.call(fn, list(choices = NULL)),
                 'must specify "choices" argument', label = fn)
  }
})

test_that("discrete scales error when `choices` is NULL", {
  fns <- c(
    "scale_pattern_type_discrete",
    "scale_pattern_subtype_discrete",
    "scale_pattern_filename_discrete",
    "scale_pattern_filter_discrete",
    "scale_pattern_gravity_discrete",
    "scale_pattern_orientation_discrete",
    "scale_pattern_grid_discrete",
    "scale_pattern_units_discrete"
  )
  for (fn in fns) {
    expect_error(do.call(fn, list(choices = NULL)),
                 'must specify "choices" argument', label = fn)
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

test_that("continuous scales error when `range` is NULL", {
  fns <- c(
    "scale_pattern_angle_continuous",
    "scale_pattern_density_continuous",
    "scale_pattern_spacing_continuous",
    "scale_pattern_xoffset_continuous",
    "scale_pattern_yoffset_continuous",
    "scale_pattern_aspect_ratio_continuous",
    "scale_pattern_key_scale_factor_continuous",
    "scale_pattern_scale_continuous",
    "scale_pattern_phase_continuous",
    "scale_pattern_frequency_continuous",
    "scale_pattern_res_continuous",
    "scale_pattern_rot_continuous"
  )
  for (fn in fns) {
    expect_error(do.call(fn, list(range = NULL)),
                 'must specify "range" argument', label = fn)
  }
})

test_that("discrete scales error when `range` is NULL", {
  fns <- c(
    "scale_pattern_angle_discrete",
    "scale_pattern_density_discrete",
    "scale_pattern_spacing_discrete",
    "scale_pattern_xoffset_discrete",
    "scale_pattern_yoffset_discrete",
    "scale_pattern_aspect_ratio_discrete",
    "scale_pattern_key_scale_factor_discrete",
    "scale_pattern_scale_discrete",
    "scale_pattern_phase_discrete",
    "scale_pattern_frequency_discrete",
    "scale_pattern_res_discrete",
    "scale_pattern_rot_discrete"
  )
  for (fn in fns) {
    expect_error(do.call(fn, list(range = NULL)),
                 'must specify "range" argument', label = fn)
  }
})

test_that("continuous identity scales work", {
  fns <- c(
    "scale_pattern_angle_identity",
    "scale_pattern_density_identity",
    "scale_pattern_spacing_identity",
    "scale_pattern_xoffset_identity",
    "scale_pattern_yoffset_identity",
    "scale_pattern_alpha_identity",
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
