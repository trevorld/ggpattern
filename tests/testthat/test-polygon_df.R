test_that("`create_polygon_df()` and `is_polygon_df()` are deprecated", {
    withr::local_options(lifecycle_verbosity = "warning")
    expect_warning(df <- create_polygon_df(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)),
                   class = "lifecycle_warning_deprecated")
    expect_equal(df, data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0), id = 1L))
    expect_warning(expect_true(is_polygon_df(df)), class = "lifecycle_warning_deprecated")
})
