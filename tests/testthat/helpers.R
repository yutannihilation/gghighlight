expect_equal_layer <- function(x, y) {
  x$mapping <- x$mapping[sort(names(x$mapping))]
  y$mapping <- y$mapping[sort(names(y$mapping))]
  x$data <- tibble::as_tibble(x$data[, sort(colnames(x$data))])
  y$data <- tibble::as_tibble(y$data[, sort(colnames(y$data))])
  expect_equal(!!as.list(x), !!as.list(y), ignore_formula_env = TRUE)
}

expect_equal_layers <- function(x, y) {
  purrr::walk2(x, y, expect_equal_layer)
}
