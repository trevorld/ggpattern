

test_polygon_df <- create_polygon_df(x=c(0, 0.3, 0.3, 0,  0.5, 0.9, 0.9, 0.5),
                                       y=c(0, 0, 0.3, 0.3,  0.5, 0.5, 0.9, 0.9),
                                       id = c(1, 1, 1, 1, 2, 2, 2, 2))


explicit_closed_polygon_df_swapped <-  create_polygon_df(x=c(0.5, 0.5, 0.9, 0.9, 0.5,   0, 0, 0.3, 0.3, 0),
                                                           y=c(0.5, 0.9, 0.9, 0.5, 0.5,   0, 0.3, 0.3, 0, 0),
                                                           id = c(1, 1, 1, 1, 1,    2, 2, 2, 2, 2))


test_that("boundary", {

  sfob <- NULL
  sfob <- convert_polygon_df_to_polygon_sf(test_polygon_df)

  res  <- convert_polygon_sf_to_polygon_df(sfob)
  expect_equal(res, explicit_closed_polygon_df_swapped)


  sfob <- convert_polygon_df_to_polygon_sf(explicit_closed_polygon_df_swapped)
  res  <- convert_polygon_sf_to_polygon_df(sfob)
  expect_equal(res, explicit_closed_polygon_df_swapped)


  grob <- convert_polygon_df_to_polygon_grob(test_polygon_df, gp = gpar(fill = 'black'))


})
