context("test-geom_highlight_preset.R")
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
d_sieved_ungrouped <- d[d$value > 1, ]
d_sieved_grouped <- d[d$type != "a", ]

test_that("geom_point_highlight() is geom_point() + geom_highlight()", {
  p1 <- ggplot(d, aes(x, y, colour = type))
  p2 <- ggplot(d)
  p3 <- ggplot(mapping = aes(x, y, colour = type))
  p4 <- ggplot()

  expect_equal(p1 + geom_point() + geom_highlight(mean(value) > 1),
               p1 + geom_point_highlight(mean(value) > 1))
  expect_equal(p2 + geom_point(aes(x, y, colour = type)) + geom_highlight(mean(value) > 1),
               p2 + geom_point_highlight(mean(value) > 1, aes(x, y, colour = type)))
  expect_equal(p3 + geom_point(data = d) + geom_highlight(mean(value) > 1),
               p3 + geom_point_highlight(mean(value) > 1, data = d))
  expect_equal(p3 + geom_point(aes(x, y, colour = type), d) + geom_highlight(mean(value) > 1),
               p3 + geom_point_highlight(mean(value) > 1, aes(x, y, colour = type), d))
})

test_that("geom_line_highlight() is geom_line() + geom_highlight()", {
})
