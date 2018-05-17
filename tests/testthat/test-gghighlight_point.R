context("gghighlight_point")

skip()

d <- data.frame(
  x     = c( 1, 1, 1, 2, 2, 2, 3, 3, 3),
  y     = c( 1, 2, 3, 1, 2, 3, 1, 2, 3),
  value = c( 1, 2, 3, 9, 8, 4, 5, 7, 6),
  category = letters[1:9],
  stringsAsFactors = FALSE
)

test_that("gghighlight_point() works", {
  expect_silent(p <- gghighlight_point(d, aes(x, y), value > 5, label_key = category))
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(length(d_built$data), 3L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_point <- d_built$data[[2]]
  data_highlighted_label <- d_built$data[[3]]

  # check if the unhighlighted data the same as the original one
  expect_equal(nrow(data_unhighlighted), 9L)
  expect_true(all(data_unhighlighted$group == -1L))

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_point), 4L)
  expect_true(all(data_highlighted_point$group == -1L))

  # check if the data for label is as expected
  expect_equal(nrow(data_highlighted_label), 4L)
  expect_true(all(data_highlighted_label$group == -1L))
})

test_that("gghighlight_point() without label_key works", {
  expect_warning(p <- gghighlight_point(d, aes(x, y), value > 5))
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(d_built$plot$layers[[3]]$mapping$label, as.name("category"))

  expect_equal(length(d_built$data), 3L)
})

test_that("gghighligt_point() works with numerical predicate", {
  expect_silent(p <- gghighlight_point(d, aes(x, y), value, max_highlight = 3L, label_key = category))
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(length(d_built$data), 3L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_point <- d_built$data[[2]]
  data_highlighted_label <- d_built$data[[3]]

  # check if the unhighlighted data the same as the original one
  expect_equal(nrow(data_unhighlighted), 9L)
  expect_true(all(data_unhighlighted$group == -1L))

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_point), 3L)
  expect_true(all(data_highlighted_point$group == -1L))

  # check if the data for label is as expected
  expect_equal(nrow(data_highlighted_label), 3L)
  expect_equal(sort(data_highlighted_label$label), sort(c("d", "e", "h")))
  expect_true(all(data_highlighted_label$group == -1L))
})
