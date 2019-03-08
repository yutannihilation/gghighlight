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

# tests -------------------------------------------------------------------

expect_equal_sieved <- function(layer, params, ..., expect) {
  params <- purrr::update_list(params, ...)
  params$layer <- layer
  expect_true(do.call(sieve_layer, params))
  expect_equal(!!layer, !!expect)
}

test_that("sieve_layer() works with simple ungrouped cases", {
  params <- list(
    group_info = NULL,
    predicates = list(rlang::quo(value > 1))
  )

  # Basic usage
  expect_equal_sieved(
    geom_bar(aes(x = x), d),
    params = params,
    expect = geom_bar(aes(x = x), d[d$value > 1, ])
  )

  # Large number of max_highlights doesn't affect the result.
  expect_equal_sieved(
    geom_bar(aes(x = x), d),
    params = params, max_highlight = 100L,
    expect = geom_bar(aes(x = x), d[d$value > 1, ])
  )

  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal_sieved(
    geom_bar(aes(x = x), d),
    params = params, max_highlight = 2L,
    expect = geom_bar(aes(x = x), d[d$value > 1, ])
  )

  # When predicate is numerical, the result is sliced.
  expect_equal_sieved(
    geom_bar(aes(x = x), d),
    params = params,
    predicates = list(rlang::quo(value)),
    max_highlight = 2L,
    expect = geom_bar(aes(x = x), d[6:7, ])
  )
})

test_that("sieve_layer() works with simple grouped cases", {
  params <- list(
    group_info = list(data = d_, id = ids, key = aes(colour = type)),
    predicates = list(rlang::quo(mean(value) > 1))
  )

  # Basic usage.
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params,
    expect = geom_bar(aes(colour = type), d[d$type != "a", ])
  )

  # Large number of max_highlights doesn't affect the result.
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params, max_highlight = 100L,
    expect = geom_bar(aes(colour = type), d[d$type != "a", ])
  )

  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params, max_highlight = 1L,
    expect = geom_bar(aes(colour = type), d[d$type != "a", ])
  )

  # When predicate is numerical, the result is sliced.
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params,
    predicates = list(rlang::quo(mean(value))),
    max_highlight = 1L,
    expect = geom_bar(aes(colour = type), d[6:7, ])
  )
})

test_that("sieve_layer() works with intentionally ungrouped cases", {
  # cases where data can be grouped, but intentionally avoid group_by;
  params <- list(
    group_info = list(data = d_, id = ids, key = rlang::quo(type)),
    predicates = list(rlang::quo(value > 1)),
    use_group_by = FALSE
  )

  # the result is same no matter group_key is provided or not
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params,
    expect = geom_bar(aes(colour = type), d[d$value > 1, ])
  )
  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params,
    group_info = list(data = d_, id = ids),
    expect = geom_bar(aes(colour = type), d[d$value > 1, ])
  )

  # even if use_group_by = TRUE, this succeeds with a warning
  expect_warning(
    expect_equal_sieved(
      geom_bar(aes(colour = type), d),
      params = params,
      expect = geom_bar(aes(colour = type), d[d$value > 1, ]),
      use_group_by = TRUE
    )
  )

  # use_group_by=TRUE without group_key generates a warning, and do sieving in ungrouped-manner.
  expect_warning(
    expect_equal_sieved(
      geom_bar(aes(colour = type), d),
      params = params,
      group_info = list(data = d_, id = ids),
      expect = geom_bar(aes(colour = type), d[d$value > 1, ]),
      use_group_by = TRUE
    )
  )
})
test_that("sieve_layer() can handle predicates that contains the group key (c.f. #27)", {
  m <- c(a = 1, b = 100, c = 10)
  params <- list(
    group_info = list(data = d_, id = ids, key = aes(colour = type)),
    predicates = list(rlang::quo(max(value * m[type]) >= 100))
  )

  expect_equal_sieved(
    geom_bar(aes(colour = type), d),
    params = params,
    expect = geom_bar(aes(colour = type), d[d$type != "a", ])
  )
})

test_that("sieve_layer() returns false if all calculation is failed", {
  expect_false(sieve_layer(geom_bar(aes(x = x), d), NULL, list(rlang::quo(no_such_column > 1))))
})

test_that("sieve_layer() works with zero predicate", {
  params <- list(group_info = NULL, predicates = NULL)

  expect_equal_sieved(
    geom_bar(aes(x = x), d),
    params = params,
    expect = geom_bar(aes(x = x), d)
  )
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

  params <- list(
    group_info = list(data = d2_, id = ids2, key = aes(colour = type))
  )

  # logical predicates only; max_highlight is ignored.
  expect_equal_sieved(
    geom_line(aes(colour = type), d2),
    params = params,
    predicates = pred_grouped[1:2],
    max_highlight = 2L,
    expect = geom_line(aes(colour = type), d2[!d2$type %in% c("a", "d"), ])
  )

  # include numerical predicates; max_highlight is used
  expect_equal_sieved(
    geom_line(aes(colour = type), d2),
    params = params,
    predicates = pred_grouped,
    max_highlight = 2L,
    expect = geom_line(aes(colour = type), d2[c(3,4,9,10), ])
  )
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
  expect_true(
    sieve_layer(
      sl,
      group_info = NULL,
      predicates = rlang::quos(p1 = l, p2 = v),
      max_highlight = 2L
    )
  )
  expect_identical(sl$mapping, aes(x))
  expect_identical(sl$data, d3[3:4, ])

  # grouped
  sl <- geom_line(aes(x, v, colour = z), d3)
  expect_true(
    sieve_layer(
      sl,
      group_info = list(data = setNames(d3[3], c("colour")),
                        id = c(1, 1, 2, 2),
                        key = aes(colour = z)),
      predicates = rlang::quos(p1 = list(l), p2 = sum(v)),
      max_highlight = 1L,
      use_group_by = TRUE
    )
  )
  expect_identical(sl$mapping, aes(x, v, colour = z))
  expect_identical(sl$data, d3[3:4, ])
})

test_that("sieve_layer() do not count NA for max_highlights", {
  d4 <- data.frame(x = c(1:3, NA))
  
  params <- list(
    group_info = NULL,
    predicates = list(rlang::quo(x))
  )
  
  expect_equal_sieved(
    geom_bar(aes(x = x), d4),
    params = params, max_highlight = 2L,
    expect = geom_bar(aes(x = x), d4[2:3, , drop = FALSE])
  )
})
