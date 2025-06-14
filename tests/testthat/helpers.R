expect_equal_layer <- function(x, y) {
  # Remove constructor, which was added in ggplot2 3.4
  x$constructor <- NULL
  y$constructor <- NULL

  x$aes_params <- x$aes_params[sort(names(x$aes_params))]
  y$aes_params <- y$aes_params[sort(names(y$aes_params))]
  x$mapping <- x$mapping[sort(names(x$mapping))]
  y$mapping <- y$mapping[sort(names(y$mapping))]
  x$data <- tibble::as_tibble(x$data[, sort(colnames(x$data))])
  y$data <- tibble::as_tibble(y$data[, sort(colnames(y$data))])
  expect_equal(
    as_no_label_list(x),
    as_no_label_list(y),
    ignore_formula_env = TRUE
  )
}

as_no_label_list <- function(x) {
  lapply(x, \(x) {
    attr(x, "label") <- NULL
    x
  })
}

expect_equal_layers <- function(x, y) {
  purrr::walk2(x, y, expect_equal_layer)
}
