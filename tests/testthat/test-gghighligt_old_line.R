context("gghighligt_line")

d <- data.frame(
  idx   = c( 1, 1, 1, 2, 2, 2, 3, 3, 3),
  value = c( 1, 2, 3,10,11,12, 9,10,11),
  category = rep(c("a","b","c"), 3),
  stringsAsFactors = FALSE
)


test_that("gghighligt_line() with usual arguments works", {
  expect_warning(
    p <- gghighlight_line(d, aes(idx, value, colour = category), max(value) > 10),
    "'gghighlight_line' is deprecated.", fixed = TRUE
  )
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
  expect_equal(data_highlighted_label$label, c("b", "c"))
})

test_that("gghighligt_line() without direct labeling works", {
  expect_warning(
    p <- gghighlight_line(d, aes(idx, value, colour = category), max(value) > 10,
                                      use_direct_label = FALSE),
    "'gghighlight_line' is deprecated.", fixed = TRUE
  )
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
  expect_true(all(data_unhighlighted$group == -1L))

  # check if the highlited data is as expected
  expect_equal(nrow(data_highlighted_line), 9L)
  expect_true(all(data_highlighted_line$group == -1L))
})

library(ggplot2)

test_that("gghighligt_line() works with facets", {
  expect_warning(
    p <- gghighlight_line(d, aes(idx, value, colour = category), max(value) > 10) + facet_wrap(~category),
    "'gghighlight_line' is deprecated.", fixed = TRUE
  )
  d_built <- ggplot2::ggplot_build(p)

  expect_equal(length(d_built$data), 3L)

  data_unhighlighted <- d_built$data[[1]]
  data_highlighted_line <- d_built$data[[2]]
  data_highlighted_label <- d_built$data[[3]]

  # check if the unhighlighted data exists on all panels.
  data_unhighlighted_split <- split(data_unhighlighted, data_unhighlighted$PANEL)
  expect_equal(length(data_unhighlighted_split), 2L)
  expect_equivalent(lapply(data_unhighlighted_split, nrow), list(9L, 9L))
  expect_equivalent(lapply(data_unhighlighted_split, function(x) length(unique(x$group))), list(3L, 3L))

  # check if the highlited data is as expected
  data_highlighted_line_split <- split(data_highlighted_line, data_highlighted_line$PANEL)
  expect_equivalent(lapply(data_highlighted_line_split, nrow), list(3L, 3L))
  expect_equivalent(lapply(data_highlighted_line_split, function(x) length(unique(x$group))), list(1L, 1L))

  # check if the data for label is as expected
  data_highlighted_label_split <- split(data_highlighted_label, data_highlighted_label$PANEL)
  expect_equivalent(lapply(data_highlighted_label_split, getElement, name = "label"), list("b", "c"))
})

test_that("gghighligt_line() raises error if use_group_by = TRUE but predicate returns multiple values per group", {
  expect_error(p <- gghighlight_line(d, aes(idx, value, colour = category), value))
  expect_error(p <- gghighlight_line(d, aes(idx, value, colour = category), value > 0))
})

test_that("gghighligt_line() works with numerical predicate", {
  expect_warning(
    p <- gghighlight_line(d, aes(idx, value, colour = category), max(value), max_highlight = 2L),
    "'gghighlight_line' is deprecated.", fixed = TRUE
  )
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
  expect_equal(data_highlighted_label$label, c("b", "c"))
})
