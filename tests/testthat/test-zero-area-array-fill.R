test_that("Zero area array fills work", {

  skip_if_not_installed("dplyr")
  skip_if_not_installed("magick")
  library(dplyr)
  library(ggplot2)

  set.seed(1234)
  ORANGE <- "#ff8a3f"
  PINK   <- "#fe0060"

  data <- data.frame(
    x = c(1, 2 ,3),
    count = c(10, 5, 0)
  )

  p <- ggplot() +
    geom_col_pattern(
      data = data,
      aes(
        x = x,
        y = count
      ),
      pattern_fill = PINK,
      pattern_fill2 = ORANGE,
      pattern = "gradient",
      colour = "#FFFFFF"
    ) +
    labs(
      title = "All heights positive, works well."
    )

  # Rendering zero area array fills used to cause an error.
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, p)


  expect_true(TRUE)

})
