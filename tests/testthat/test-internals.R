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
