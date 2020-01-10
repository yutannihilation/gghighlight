expect_equal_layer <- function(x, y) {
  x$mapping <- x$mapping[sort(names(x$mapping))]
  y$mapping <- y$mapping[sort(names(y$mapping))]
  expect_equal(x, y)
}

expect_equal_layers <- function(x, y) {
  purrr::walk2(x, y, expect_equal_layer)
}
