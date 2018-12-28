context("test-internals.R")

test_that("choose_col_for_filter_and_arrange() works", {
  d1 <- tibble::tibble(x = 1,
                       y = "a",
                       z = factor("a"),
                       p1 = TRUE,
                       lst = list(1, 2))

  expected <- list(filter = rlang::syms(c("p1")),
                   arrange = rlang::syms(c("y", "z")))
  expect_equal(choose_col_for_filter_and_arrange(d1, rlang::sym("x")),
               !!expected)

  expect_equal(choose_col_for_filter_and_arrange(d1, rlang::quo(x)),
               !!expected)
})

test_that("normalize_unhighlighted_params() works", {
  expect_listequal <- function(x, y) {
    expect_equal(!!x[sort(names(x))], !!y[sort(names(y))])
  }

  # if fill and colour is specified, respect both
  expect_listequal(normalize_unhighlighted_params(list(colour = "blue", fill = "red")),
                   list(colour = "blue", fill = "red"))
  # other parameters are left as is
  expect_listequal(normalize_unhighlighted_params(list(fill = "red", size = 0.2)),
                   list(fill = "red", size = 0.2))
  # color is an alias of colour
  expect_listequal(normalize_unhighlighted_params(list(color = "red")),
                   list(colour = "red"))
  # if both colour and color are specified, use colour.
  expect_listequal(normalize_unhighlighted_params(list(colour = "blue", color = "red")),
                   list(colour = "blue"))
})
