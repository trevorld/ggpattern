test_that("zero-size legend keys don't draw a pattern", {

    df <- data.frame(trt = c("a", "b"), outcome = c(2, 3))
    p <- ggplot(df, aes(trt, outcome, pattern_fill = trt)) +
        geom_col_pattern() +
        theme(legend.key.size = unit(0, "mm"))

    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_no_error(print(p))
    expect_no_error(print(p + theme(legend.key.width = unit(5, "mm"))))
})
