context("test-sieve")

grey07 <- ggplot2::alpha("grey", 0.7)

d <- tibble::tribble(
  ~x, ~y, ~type, ~value,
  1,  2,   "a",      0,
  2,  3,   "a",      1,
  3,  4,   "a",      0,
  1,  3,   "b",      1,
  2,  2,   "b",      5,
  1,  4,   "c",     10,
  3,  3,   "c",     10
)

d_ <- setNames(d[1:3], c("x", "y", "colour"))
ids <- c(1, 1, 1, 2, 2, 3, 3)
g_info <- list(data = d_, id = ids, key = aes(colour = type))

pred_ungrouped <- list(rlang::quo(value > 1))
pred_grouped <- list(rlang::quo(mean(value) > 1))
d_sieved_ungrouped <- d[d$value > 1, ]
d_sieved_grouped <- d[d$type != "a", ]

# tests -------------------------------------------------------------------

test_that("sieve_layer() works with simple cases", {
  # Ungrouped.
  f <- function(...) {
    l <- geom_bar(aes(x = x), d)
    expect_true(sieve_layer(l, NULL, pred_ungrouped, ...))
    l
  }
  # Basic usage.
  expect_equal(f(), geom_bar(aes(x = x), d_sieved_ungrouped))
  # Large number of max_highlights doesn't affect the result.
  expect_equal(f(max_highlight = 100L), geom_bar(aes(x = x), d_sieved_ungrouped))
  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal(f(max_highlight = 2L), geom_bar(aes(x = x), d_sieved_ungrouped))
  # When predicate is numerical, the result is sliced.
  l <- geom_bar(aes(x = x), d)
  expect_true(sieve_layer(l, NULL, list(rlang::quo(value)), max_highlight = 2L))
  expect_equal(l, geom_bar(aes(x = x), d[6:7, ]))

  # Grouped.
  f <- function(...) {
    l <- geom_bar(aes(colour = type), d)
    expect_true(sieve_layer(l, g_info, pred_grouped, ...))
    l
  }
  # Basic usage.
  expect_equal(f(), geom_bar(aes(colour = type), d_sieved_grouped))
  # Large number of max_highlights doesn't affect the result.
  expect_equal(f(max_highlight = 100L), geom_bar(aes(colour = type), d_sieved_grouped))
  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal(f(max_highlight = 1L), geom_bar(aes(colour = type), d[d$type != "a", ]))
  # When predicate is numerical, the result is sliced.
  l <- geom_bar(aes(colour = type), d)
  expect_true(sieve_layer(l, g_info, list(rlang::quo(mean(value))), max_highlight = 1L))
  expect_equal(l, geom_bar(aes(colour = type), d[6:7, ]))

  # can be grouped, but intentionally avoid group_by;
  # the result is same no matter group_key is provided or not
  f <- function (key, use_group_by) {
    info <- g_info
    info$key <- key
    l <- geom_bar(aes(colour = type), d)
    expect_true(sieve_layer(l, info, pred_ungrouped, use_group_by = use_group_by))
    l
  }
  expect_equal(f(rlang::quo(type), use_group_by = FALSE), geom_bar(aes(colour = type), d_sieved_ungrouped))
  expect_equal(f(NULL, FALSE), geom_bar(aes(colour = type), d_sieved_ungrouped))
  # even if use_group_by = TRUE, this succeeds with a warning
  expect_warning(l <- f(rlang::quo(type), use_group_by = TRUE))
  expect_equal(l, geom_bar(aes(colour = type), d_sieved_ungrouped))

  # use_group_by=TRUE without group_key generates a warning, and do sieving in ungrouped-manner.
  l <- geom_bar(aes(x = x), d)
  expect_warning(sieve_layer(l, NULL, pred_ungrouped, use_group_by = TRUE))
  expect_equal(l, geom_bar(aes(x = x), d_sieved_ungrouped))

  # predicate can contain group key (c.f. #27)
  m <- c(a = 1, b = 100, c = 10)
  pred_use_group_var <- list(rlang::quo(max(value * m[type]) >= 100))
  l <- geom_bar(aes(colour = type), d)
  expect_true(sieve_layer(l, g_info, pred_use_group_var))
  expect_equal(l, geom_bar(aes(colour = type), d[d$type != "a", ]))
})

test_that("sieve_layer() returns false if all calculation is failed", {
  expect_false(sieve_layer(geom_bar(aes(x = x), d), NULL, list(rlang::quo(no_such_column > 1))))
})

test_that("sieve_layer() works with zero predicate", {
  l <- geom_bar(aes(x = x), d)
  expect_true(sieve_layer(l, NULL, list()))
  expect_equal(l, geom_bar(aes(x = x), d))
})

test_that("sieve_layer() works with more than two predicates", {
  d2 <- tibble::tribble(
    ~type, ~val1, ~val2,
    "a",     1,     0,
    "a",     2,     2,
    "b",     4,     2,
    "b",     5,     2,
    "c",    10,     0,
    "c",    10,     2,
    "d",    10,     1,
    "d",    10,     3,
    "e",    11,    10,
    "e",    12,    30
  )
  pred_grouped <- rlang::quos(
    mean(val1) > 2,      # logical to filter out "a"
    any(val2 %% 2 == 0), # logical to filter out "d"
    sum(val2)            # numerical
  )

  d2_ <- setNames(d2[1], c("colour"))
  ids2 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  g_info2 <- list(data = d2_, id = ids2, key = aes(colour = type))


  # logical predicates only; max_highlight is ignored.
  l <- geom_line(aes(colour = type), d2)
  expect_true(sieve_layer(l, g_info2, pred_grouped[1:2], max_highlight = 2))
  expect_equal(l, geom_line(aes(colour = type), d2[!d2$type %in% c("a", "d"), ]))

  l <- geom_line(aes(colour = type), d2)
  expect_true(sieve_layer(l, g_info2, pred_grouped, max_highlight = 2))
  expect_equal(l, geom_line(aes(colour = type), d2[c(3,4,9,10), ]))
})

test_that("sieve_layer() works with list columns", {
  d3 <- tibble::tibble(
    x = 1:4,
    v = 1:4,
    z = c("a", "a", "b", "b"),
    l = list(
      c(1, 2, 3),
      c(1, 4, 10),
      c(4, 6),
      c(1)
    )
  )

  # ungrouped
  sl <- geom_bar(aes(x), d3)
  expect_true(sieve_layer(sl, NULL, rlang::quos(p1 = l, p2 = v), max_highlight = 2))
  expect_identical(sl$mapping, aes(x))
  expect_identical(sl$data, d3[3:4, ])

  # grouped
  d3_ <- setNames(d3[3], c("colour"))
  ids3 <- c(1, 1, 2, 2)
  group_info3 <- list(data = d3_, id = ids3, key = aes(colour = z))
  sl <- geom_line(aes(x, v, colour = z), d3)
  expect_true(
    sieve_layer(sl, group_info3, rlang::quos(p1 = list(l), p2 = sum(v)), max_highlight = 1, use_group_by = TRUE)
  )
  expect_identical(sl$mapping, aes(x, v, colour = z))
  expect_identical(sl$data, d3[3:4, ])
})
