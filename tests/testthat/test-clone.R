context("test-clone.R")
library(ggplot2)

d <- data.frame(x = 1:10, y = 1:10, cat = rep(1:2, each = 5))

test_that("clone_layer() clones a layer properly", {
  p <- ggplot(d, aes(x, y)) +
    geom_point(aes(group = x), color = "red")

  l <- clone_layer(p$layers[[1]])
  expect_equal(l, p$layers[[1]])
})

test_that("rename_variables() renames a mapping properly", {
  expect_equal(rename_variables(NULL), NULL)
  expect_equal(rename_variables(aes(x = aa)), aes(x = aa))
  expect_equal(rename_variables(aes(x = aa, color = zz)), aes(x = aa, colour = zz))
})

test_that("update_layer() updates a layer properly", {
  d2 <- data.frame(x = 1)
  f <- function(data = d2, mapping = aes(x = foo, y = bar), colour = "baz", ...) {
    geom_bar(data = data, mapping = mapping, colour = colour, ...)
  }

  expect_equal(update_layer(f(), data = d, mapping = NULL, aes_params = list()),
               f(data = d))
  expect_equal(update_layer(f(), data = NULL, mapping = aes(x = x), aes_params = list()),
               f(mapping = aes(x = x)))
  expect_equal(update_layer(f(), data = NULL, mapping = NULL, aes_params = list(color = "red", fill = "blue")),
               f(colour = "red", fill = "blue"))
})

test_that("geom_clone() clones the upper layers as it is", {
  p <- ggplot(d, aes(x, y)) +
    geom_point(aes(group = x), color = "red")

  # one layer
  p1 <- p + geom_clone()

  expect_equal(length(p1$layers), 2)
  expect_s3_class(p1$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p1$layers[[2]]$geom, "GeomPoint")
  expect_equal(p1$layers[[1]]$aes_params, list(colour = "red"))
  expect_equal(p1$layers[[2]]$aes_params, list(colour = "red"))
  expect_equal(p1$layers[[1]]$mapping, aes(group = x))
  expect_equal(p1$layers[[2]]$mapping, aes(group = x))

  p <- p + geom_line(aes(group = cat), linetype = "dotted")

  # two layer
  p2 <- p + geom_clone()

  expect_equal(length(p2$layers), 4)
  expect_s3_class(p2$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p2$layers[[2]]$geom, "GeomLine")
  expect_s3_class(p2$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(p2$layers[[4]]$geom, "GeomLine")
  expect_equal(p2$layers[[1]]$aes_params, list(colour = "red"))
  expect_equal(p2$layers[[2]]$aes_params, list(linetype = "dotted"))
  expect_equal(p2$layers[[3]]$aes_params, list(colour = "red"))
  expect_equal(p2$layers[[4]]$aes_params, list(linetype = "dotted"))
  expect_equal(p2$layers[[1]]$mapping, aes(group = x))
  expect_equal(p2$layers[[2]]$mapping, aes(group = cat))
  expect_equal(p2$layers[[3]]$mapping, aes(group = x))
  expect_equal(p2$layers[[4]]$mapping, aes(group = cat))

  # two layer, but clones only one
  p3 <- p + geom_clone(n = 1)

  expect_equal(length(p3$layers), 3)
  expect_s3_class(p3$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p3$layers[[2]]$geom, "GeomLine")
  expect_s3_class(p3$layers[[3]]$geom, "GeomLine")
  expect_equal(p3$layers[[1]]$aes_params, list(colour = "red"))
  expect_equal(p3$layers[[2]]$aes_params, list(linetype = "dotted"))
  expect_equal(p3$layers[[3]]$aes_params, list(linetype = "dotted"))
  expect_equal(p3$layers[[1]]$mapping, aes(group = x))
  expect_equal(p3$layers[[2]]$mapping, aes(group = cat))
  expect_equal(p3$layers[[3]]$mapping, aes(group = cat))
})

test_that("geom_clone() clones the upper layers and modifies the parameters", {
  d2 <- data.frame(x = 1)

  p <- ggplot(d) +
    geom_point(aes(group = x), color = "red") +
    geom_line()

  # two layers
  p1 <- p + geom_clone(data = d2)

  expect_equal(length(p1$layers), 4)
  expect_s3_class(p1$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p1$layers[[2]]$geom, "GeomLine")
  expect_s3_class(p1$layers[[3]]$geom, "GeomPoint")
  expect_s3_class(p1$layers[[4]]$geom, "GeomLine")
  expect_equal(p1$layers[[1]]$data, waiver())
  expect_equal(p1$layers[[2]]$data, waiver())
  expect_equal(p1$layers[[3]]$data, d2)
  expect_equal(p1$layers[[4]]$data, d2)
})

test_that("geom_clone() throws error if there is no upper layers", {
  p <- ggplot(d, aes(x, y))

  expect_error(p + geom_clone())
})
