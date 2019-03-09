context("test-keep-scale")

test_that("gghighlight(keep_scale = TRUE) keeps the original scale", {
  # by default, the scale is not kept
  p1 <- ggplot(data.frame(x = 1:10)) + geom_point(aes(x, x, fill = x)) + gghighlight(x > 5)
  b1 <- ggplot_build(p1)

  expect_equal(b1$plot$scales$get_scales("fill")$range$range, c(6L, 10L))

  # if keep_scale = TRUE, the original scale is kept
  p2 <- ggplot(data.frame(x = 1:10)) + geom_point(aes(x, x, fill = x)) + gghighlight(x > 5, keep_scale = TRUE)
  b2 <- ggplot_build(p2)

  expect_equal(b2$plot$scales$get_scales("fill")$range$range, c(1L, 10L))
})
