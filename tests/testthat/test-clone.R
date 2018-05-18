context("test-clone.R")

d <- data.frame(x = 1:10, y = 1:10, cat = rep(1:2, each = 5))

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
  expect_equal(p1$layers[[1]]$aes_params, aes(group = x))
  expect_equal(p1$layers[[2]]$aes_params, aes(group = x))

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
  expect_equal(p2$layers[[1]]$aes_params, aes(group = x))
  expect_equal(p2$layers[[2]]$aes_params, aes(group = cat))
  expect_equal(p2$layers[[3]]$aes_params, aes(group = x))
  expect_equal(p2$layers[[4]]$aes_params, aes(group = cat))

  # two layer, but clones only one
  p3 <- p + geom_clone(layers = 1)

  expect_equal(length(p3$layers), 3)
  expect_s3_class(p3$layers[[1]]$geom, "GeomPoint")
  expect_s3_class(p3$layers[[2]]$geom, "GeomLine")
  expect_s3_class(p3$layers[[3]]$geom, "GeomLine")
  expect_equal(p3$layers[[1]]$aes_params, list(colour = "red"))
  expect_equal(p3$layers[[2]]$aes_params, list(linetype = "dotted"))
  expect_equal(p3$layers[[3]]$aes_params, list(linetype = "dotted"))
  expect_equal(p3$layers[[1]]$aes_params, aes(group = x))
  expect_equal(p3$layers[[2]]$aes_params, aes(group = cat))
  expect_equal(p3$layers[[3]]$aes_params, aes(group = cat))
})

test_that("geom_clone() clones the upper layers and modifies the parameters", {
  p <- ggplot(d, aes(x, y)) +
    geom_point()
})

test_that("geom_clone() throws error if there is no upper layers", {
  p <- ggplot(d, aes(x, y))

  expect_error(p + geom_clone())
})
