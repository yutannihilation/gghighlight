context("test-geom_highlight.R")
library(ggplot2)

grey07 <- ggplot2::alpha("grey", 0.7)

test_that("merge_aes() works", {
  bar_aes <- geom_bar()$geom$aesthetics()
  line_aes <- geom_line()$geom$aesthetics()
  # If one is NULL, return the other one as is
  expect_equal(merge_aes(aes(colour = a), NULL, bar_aes),
               aes(colour = a))
  expect_equal(merge_aes(NULL, aes(colour = a), bar_aes),
               aes(colour = a))
  # If both are not NULL, layer_mapping is used.
  expect_equal(merge_aes(aes(x = x, colour = b), aes(colour = a, fill = c), bar_aes),
               aes(x = x, colour = b, fill = c))
  # If the layer doesn't have fill aes, it is omitted.
  expect_equal(merge_aes(aes(x = x, colour = b), aes(colour = a, fill = c), line_aes),
               aes(x = x, colour = b))
})

test_that("bleach_layer() works", {
  expect_equal(bleach_layer(geom_bar(aes(colour = a)), NULL, grey07),
               geom_bar(aes(group = a), colour = grey07))

  # when colour and fill is specified at the same time, fill is used as the group key
  expect_equal(bleach_layer(geom_bar(aes(colour = a, fill = b)), NULL, grey07),
               geom_bar(aes(group = b), colour = grey07, fill = grey07))

  # if the fill aes belongs to the plot, the result is the same as above.
  expect_equal(bleach_layer(geom_bar(aes(colour = a)), aes(fill = b), grey07),
               geom_bar(aes(group = b), colour = grey07, fill = grey07))

  # but if the geom doesn't have fill aes, colour is used as the group key.
  expect_equal(bleach_layer(geom_line(aes(colour = a)), aes(fill = b), grey07),
               geom_line(aes(group = a), colour = grey07))
})

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

expect_one_layer <- function(p) {
  mapping_orig <- p$layers[[1]]$mapping
  data_orig <- p$layers[[1]]$data

  p <- p + geom_highlight(mean(value) > 1)

  expect_equal(length(p$layers), 2L)

  class_expected <- c("GeomLine", "GeomLine")
  for (i in 1:2) {
    expect_s3_class(p$layers[[!!i]]$geom, class_expected[!!i])
  }
  # the original layer should be greyed out
  expect_equal(p$layers[[1]]$data, data_orig)
  # Group aes is needed because otherwise lines are not grouped without colour aes.
  expect_equal(p$layers[[1]]$mapping, aes(x = x, y = y, group = type))
  expect_equal(p$layers[[1]]$aes_params, list(colour = ggplot2::alpha("grey", 0.7)))

  # the new layer should be highlighted
  expect_equal(p$layers[[2]]$data, d[d$type != "a", ])
  expect_equal(p$layers[[2]]$mapping, mapping_orig)
  expect_equal(p$layers[[2]]$aes_params, list())
}

test_that("geom_highlight() works when both the data and the aes belong to the plot", {
  p <- ggplot(d, aes(x, y, colour = type)) +
    geom_line()

  expect_one_layer(p)
})

test_that("geom_highlight() works when the data belongs to the plot and the aes belongs to the layer", {
  p <- ggplot(d) +
    geom_line(aes(x, y, colour = type))

})

test_that("geom_highlight() works when both the data and the aes belongs to the layer", {
  p <- ggplot() +
    geom_line(data = d, aes(x, y, colour = type))
})
