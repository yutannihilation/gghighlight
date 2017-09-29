context("gghighligt_line")

d <- data.frame(
  idx = c( 1, 1, 1, 2, 2, 2, 3, 3, 3),
  value = c( 1, 2, 3,10,11,12, 9,10,11),
  category = rep(c("a","b","c"), 3),
  stringsAsFactors = FALSE
)


test_that("gghighligt_line() with usual arguments works", {
  expect_silent(p <- gghighlight_line(d, aes(idx, value, colour = category), max(value) > 10))
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(length(d_built$data), 3L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_line <- d_built$data[[2]]
  data_highlighted_label <- d_built$data[[3]]

  # check if the unhighlighted data the same as the original one
  expect_equal(nrow(data_unhighlighted), 9L)
  expect_equal(length(unique(data_unhighlighted$group)), 3L)

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_line), 6L)
  expect_equal(length(unique(data_highlighted_line$group)), 2L)

  # check if the data for label is as expected
  expect_equal(nrow(data_highlighted_label), 2L)
  expect_equal(length(unique(data_highlighted_label$group)), 2L)
})

test_that("gghighligt_line() without direct labeling works", {
  expect_silent(p <- gghighlight_line(d, aes(idx, value, colour = category), max(value) > 10,
                                      use_direct_label = FALSE))
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(length(d_built$data), 2L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_line <- d_built$data[[2]]

  # check if the unhighlighted data the same as the original one
  expect_equal(nrow(data_unhighlighted), 9L)
  expect_equal(length(unique(data_unhighlighted$group)), 3L)

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_line), 6L)
  expect_equal(length(unique(data_highlighted_line$group)), 2L)
})


test_that("gghighligt_line() without colour mapping works", {
  expect_warning(p <- gghighlight_line(d, aes(idx, value), max(value) > 10))
  d_built <- ggplot2::ggplot_build(p)

  # assume no labels
  expect_equal(length(d_built$data), 2L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_line <- d_built$data[[2]]

  # check if the unhighlighted data the same as the original one
  expect_equal(nrow(data_unhighlighted), 9L)
  expect_equal(length(unique(data_unhighlighted$group)), 1L)

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_line), 9L)
  expect_equal(length(unique(data_highlighted_line$group)), 1L)
})


