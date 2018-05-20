context("test-geom_highlight.R")
library(ggplot2)

grey07 <- ggplot2::alpha("grey", 0.7)

d <- tibble::tribble(
  ~x, ~y, ~type, ~value,
  1,  2,   "a",     0,
  2,  3,   "a",     1,
  3,  4,   "a",     0,
  1,  3,   "b",     1,
  2,  2,   "b",     5,
  1,  4,   "c",    10,
  3,  3,   "c",    10
)

test_that("merge_mapping() works", {
  # If both are NULL, throw error
  expect_error(merge_mapping(geom_bar(), NULL))
  # If one is NULL, return the other one as is.
  expect_equal(merge_mapping(geom_bar(aes(x = a)), NULL),
               aes(x = a))
  expect_equal(merge_mapping(geom_bar(), aes(x = a)),
               aes(x = a))
  # If both are not NULL, layer_mapping is used.
  expect_equal(merge_mapping(geom_bar(aes(x = x, colour = b)), aes(colour = a, fill = c)),
               aes(x = x, colour = b, fill = c))
  # If the layer doesn't have fill aes, it is omitted.
  expect_equal(merge_mapping(geom_line(aes(x = x, colour = b)), aes(colour = a, fill = c)),
               aes(x = x, colour = b))
})

test_that("merge_data() works", {
  # if oneare NULL, return the other one as is
  expect_equal(merge_data(geom_bar(data = d), waiver()),
               d)
  expect_equal(merge_data(geom_bar(), d),
               d)
})

test_that("bleach_layer() works", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_GROUP_COLUMN_NAME)

  aes_bleached <- aes(colour = NULL, fill = NULL, group = !!VERY_SECRET_GROUP_COLUMN_NAME)

  # If mapping doesn't have colour or fill, it raises an error.
  expect_error(bleach_layer(geom_bar(aes(x = x, y = y), d), rlang::quo(x), grey07))

  # If colour is specified, colour is used as the group key.
  expect_equal(bleach_layer(geom_bar(aes(colour = type), d), rlang::quo(type), grey07),
               geom_bar(aes_bleached, d_bleached, colour = grey07))

  # If colour is specified but group_key is NULL, the result is the same data.
  expect_equal(bleach_layer(geom_point(aes(colour = type), d), NULL, grey07),
               geom_point(aes(colour = NULL, fill = NULL), d, colour = grey07))

  # If colour and fill is specified at the same time, fill is used as the group key.
  expect_equal(bleach_layer(geom_bar(aes(colour = type, fill = type), d), rlang::quo(type), grey07),
               geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))
})

test_that("sieve_layer() works", {
  pred_ungrouped <- list(rlang::quo(value > 1))
  pred_grouped <- list(rlang::quo(mean(value) > 1))
  d_sieved_ungrouped <- d[d$value > 1, ]
  d_sieved_grouped <- d[d$type != "a", ]

  expect_equal(sieve_layer(geom_bar(aes(colour = type), d), NULL, pred_ungrouped, 5L),
               geom_bar(aes(colour = type), d_sieved_ungrouped))
  expect_equal(sieve_layer(geom_bar(aes(colour = type), d), rlang::quo(type), pred_grouped, 5L),
               geom_bar(aes(colour = type), d_sieved_grouped))
})

test_that("geom_highlight() works the plot with one layer, grouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_GROUP_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_GROUP_COLUMN_NAME)

  d_sieved <- d[d$type != "a", ]

  l_bleached <- geom_line(aes_bleached, d_bleached, colour = grey07)
  l_sieved <- geom_line(aes(x, y, colour = type), d_sieved)

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_line()

  p2 <- ggplot(d) +
    geom_line(aes(x, y, colour = type))

  p3 <- ggplot() +
    geom_line(data = d, aes(x, y, colour = type))

  expect_equal((p1 + geom_highlight(mean(value) > 1))$layers, list(l_bleached, l_sieved))
  expect_equal((p2 + geom_highlight(mean(value) > 1))$layers, list(l_bleached, l_sieved))
  expect_equal((p3 + geom_highlight(mean(value) > 1))$layers, list(l_bleached, l_sieved))
})

test_that("geom_highlight() works the plot with one layer, ungrouped", {
  skip("TODO")
})

test_that("geom_highlight() works with two layers, grouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_GROUP_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_GROUP_COLUMN_NAME)

  d_sieved <- d[d$type != "a", ]

  l_bleached_1 <- geom_line(aes_bleached, d_bleached, colour = grey07)
  l_sieved_1 <- geom_line(aes(x, y, colour = type), d_sieved)
  l_bleached_2 <- geom_point(aes_bleached, d_bleached,
                             shape = "circle filled", colour = grey07, fill = grey07)
  l_sieved_2 <- geom_point(aes(x, y, colour = type, fill = type), d_sieved,
                           shape = "circle filled")

  p1 <- ggplot(d, aes(x, y, colour = type, fill = type)) +
    geom_line() +
    geom_point(shape = "circle filled")

  expect_equal((p1 + geom_highlight(mean(value) > 1))$layers,
               list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal((p1 + geom_highlight(mean(value) > 1, n = 1))$layers,
               list(geom_line(), l_bleached_2, l_sieved_2))
})

test_that("geom_highlight() works with two layers, grouped", {
  skip("TODO")
})
