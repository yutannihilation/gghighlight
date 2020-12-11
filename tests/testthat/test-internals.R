test_that("choose_col_for_filter_and_arrange() works", {
  d1 <- tibble::tibble(x = 1,
                       y = "a",
                       z = factor("a"),
                       p1 = TRUE,
                       lst = list(1, 2))

  expected <- list(filter = c("p1"),
                   arrange = c("y", "z"))
  expect_equal(choose_col_for_filter_and_arrange(d1, sym("x")),
               !!expected)

  expect_equal(choose_col_for_filter_and_arrange(d1, quo(x)),
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


test_that("calculate_ungrouped() and calculate_grouped() don't drop data.frames", {
  d <- data.frame(x = 1:10)
  expect_equal(calculate_ungrouped(d, quos(p1 = x > 0), Inf),
               d)
  expect_equal(calculate_ungrouped(d, quos(p1 = x > 5), Inf),
               d[6:10, , drop = FALSE])

  expect_equal(calculate_grouped(d, quos(p1 = max(x) > 0), Inf, rep(1:2, each = 5)),
               d)
  expect_equal(calculate_grouped(d, quos(p1 = max(x) > 5), Inf, rep(1:2, each = 5)),
               d[6:10, , drop = FALSE])

})

test_that("get_facet_vars() extract facet specs", {
  p <- ggplot()
  v <- quos(A = a, B = b)

  expect_identical(get_facet_vars(p$facet), NULL)
  expect_identical(get_facet_vars((p + facet_wrap(v))$facet), v)
  expect_identical(get_facet_vars((p + facet_grid(v))$facet), v)
  expect_identical(get_facet_vars((p + facet_grid(v[1], v[2]))$facet), v)

  class(p$facet) <- c("FacetUnknown", class(p$facet))
  expect_error(get_facet_vars(p$facet))
})
