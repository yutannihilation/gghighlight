context("test-geom_highlight.R")
library(ggplot2)

grey07 <- ggplot2::alpha("grey", 0.7)

d <- tibble::tribble(
  ~x, ~y, ~type, ~value,
  1,  2,   "a",     0,
  2,  3,   "a",     1,
  3,  4,   "a",     0,
  1,  3,   "b",     1,
  2,  2,   "b",     5,
  1,  4,   "c",    10,
  3,  3,   "c",    10
)

test_that("merge_mapping() works", {
  # If both are NULL, throw error
  expect_error(merge_mapping(geom_bar(), NULL))
  # If one is NULL, return the other one as is.
  expect_equal(merge_mapping(geom_bar(aes(x = a)), NULL),
               aes(x = a))
  expect_equal(merge_mapping(geom_bar(), aes(x = a)),
               aes(x = a))
  # If both are not NULL, layer_mapping is used.
  expect_equal(merge_mapping(geom_bar(aes(x = x, colour = b)), aes(colour = a, fill = c)),
               aes(x = x, colour = b, fill = c))
  # If the layer doesn't have fill aes, it is omitted.
  expect_equal(merge_mapping(geom_line(aes(x = x, colour = b)), aes(colour = a, fill = c)),
               aes(x = x, colour = b))
})

test_that("merge_data() works", {
  # if oneare NULL, return the other one as is
  expect_equal(merge_data(geom_bar(data = d), waiver()),
               d)
  expect_equal(merge_data(geom_bar(), d),
               d)
})

test_that("bleach_layer() works", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)

  aes_bleached <- aes(colour = NULL, fill = NULL, group = !!VERY_SECRET_COLUMN_NAME)

  # If colour is specified, colour is used as the group key.
  expect_equal(bleach_layer(geom_line(aes(colour = type), d), rlang::quo(type), grey07),
               geom_line(aes_bleached, d_bleached, colour = grey07))

  # If colour is specified but group_key is NULL, the result is the same data.
  expect_equal(bleach_layer(geom_line(aes(colour = type), d), NULL, grey07),
               geom_line(aes(colour = NULL, fill = NULL), d, colour = grey07))

  # If the geom accepts fill, it is sets to grey even when it is not included in the mapping.
  expect_equal(bleach_layer(geom_bar(aes(colour = type), d), rlang::quo(type), grey07),
               geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If colour and fill is specified at the same time, fill is used as the group key.
  expect_equal(bleach_layer(geom_bar(aes(colour = type, fill = type), d), rlang::quo(type), grey07),
               geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If mapping doesn't have colour or fill, it's OK.
  # c.f. https://github.com/yutannihilation/gghighlight/pull/17#issuecomment-390486101.
  expect_equal(bleach_layer(geom_bar(aes(group = type), d), rlang::quo(type), grey07),
               # since group aes already exists, group comes first
               geom_bar(aes(group = !!VERY_SECRET_COLUMN_NAME, colour = NULL, fill = NULL),
                        d_bleached, colour = grey07, fill = grey07))
})

test_that("sieve_layer() works with simple cases", {
  pred_ungrouped <- list(rlang::quo(value > 1))
  pred_grouped <- list(rlang::quo(mean(value) > 1))
  d_sieved_ungrouped <- d[d$value > 1, ]
  d_sieved_grouped <- d[d$type != "a", ]

  # Ungrouped.
  f <- function(...) sieve_layer(geom_bar(aes(x = x), d), NULL, pred_ungrouped, ...)
  # Basic usage.
  expect_equal(f(), geom_bar(aes(x = x), d_sieved_ungrouped))
  # Large number of max_highlights doesn't affect the result.
  expect_equal(f(max_highlight = 100L), geom_bar(aes(x = x), d_sieved_ungrouped))
  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal(f(max_highlight = 2L), geom_bar(aes(x = x), d_sieved_ungrouped))
  # When predicate is numerical, the result is sliced.
  expect_equal(sieve_layer(geom_bar(aes(x = x), d), NULL, list(rlang::quo(value)), max_highlight = 2L),
               geom_bar(aes(x = x), d[6:7, ]))

  # Grouped.
  f <- function(...) sieve_layer(geom_bar(aes(colour = type), d), rlang::quo(type), pred_grouped, ...)
  # Basic usage.
  expect_equal(f(), geom_bar(aes(colour = type), d_sieved_grouped))
  # Large number of max_highlights doesn't affect the result.
  expect_equal(f(max_highlight = 100L), geom_bar(aes(colour = type), d_sieved_grouped))
  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal(f(max_highlight = 1L), geom_bar(aes(colour = type), d[d$type != "a", ]))
  # When predicate is numerical, the result is sliced.
  expect_equal(sieve_layer(geom_bar(aes(colour = type), d), rlang::quo(type),
                           list(rlang::quo(mean(value))), max_highlight = 1L),
               geom_bar(aes(colour = type), d[6:7, ]))

  # can be grouped, but intentionally avoid group_by;
  # the result is same no matter group_key is provided or not
  f <- function (key, use_group_by) {
    sieve_layer(geom_bar(aes(colour = type), d), key, pred_ungrouped, use_group_by = use_group_by)
  }
  expect_equal(f(rlang::quo(type), use_group_by = FALSE), geom_bar(aes(colour = type), d_sieved_ungrouped))
  expect_equal(f(NULL, FALSE), geom_bar(aes(colour = type), d_sieved_ungrouped))
  # even if use_group_by = TRUE, this succeeds with a warning
  expect_warning(l <- f(rlang::quo(type), use_group_by = TRUE))
  expect_equal(l, geom_bar(aes(colour = type), d_sieved_ungrouped))
  
  # use_group_by=TRUE without group_key generates a warning, and do sieving in ungrouped-manner.
  expect_warning(l <- sieve_layer(geom_bar(aes(x = x), d), NULL, pred_ungrouped, use_group_by = TRUE))
  expect_equal(l, geom_bar(aes(x = x), d_sieved_ungrouped))

  # predicate can contain group key (c.f. #27)
  m <- c(a = 1, b = 100, c = 10)
  pred_use_group_var <- list(rlang::quo(max(value * m[type]) >= 100))
  l <- sieve_layer(geom_bar(aes(colour = type), d), rlang::quo(type), pred_use_group_var)
  expect_equal(l, geom_bar(aes(colour = type), d[d$type != "a", ]))
})

test_that("sieve_layer() works with zero predicate", {
  expect_equal(sieve_layer(geom_bar(aes(x = x), d), NULL, list()),
               geom_bar(aes(x = x), d))
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

  # logical predicates only; max_highlight is ignored.
  expect_equal(sieve_layer(geom_line(aes(colour = type), d2), rlang::quo(type),
                           pred_grouped[1:2], max_highlight = 2),
               geom_line(aes(colour = type), d2[!d2$type %in% c("a", "d"), ]))

  expect_equal(sieve_layer(geom_line(aes(colour = type), d2), rlang::quo(type),
                           pred_grouped, max_highlight = 2),
               geom_line(aes(colour = type), d2[c(3,4,9,10), ]))
})

test_that("geom_highlight() does not change the existing layers", {
  p1 <- ggplot(d, aes(x, y, colour = type)) + geom_line()
  p2 <- ggplot(d, aes(x, y, colour = type)) + geom_line()

  invisible(p1 + geom_highlight(mean(value) > 1))

  expect_equal(p1, p2)
})

test_that("geom_highlight() works the plot with one layer, grouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_COLUMN_NAME)

  d_sieved <- d[d$type != "a", ]

  l_bleached <- geom_line(aes_bleached, d_bleached, colour = grey07)
  l_sieved <- geom_line(aes(x, y, colour = type), d_sieved)

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_line()

  p2 <- ggplot(d) +
    geom_line(aes(x, y, colour = type))

  p3 <- ggplot() +
    geom_line(data = d, aes(x, y, colour = type))

  # without labels
  for (p in list(p1, p2, p3)) {
    p_highlighted <- p + geom_highlight(mean(value) > 1, use_direct_label = FALSE)
    expect_equal(p_highlighted$data, d_sieved)
    expect_equal(p_highlighted$layers, list(l_bleached, l_sieved))
    expect_equal(p_highlighted$guides, NULL)
  }

  # with labels
  d_label <- d_sieved %>%
    dplyr::group_by(type) %>%
    dplyr::arrange(desc(x)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  l_label <- ggrepel::geom_label_repel(aes(x, y, colour = type, label = type), d_label)
  for (p in list(p1, p2, p3)) {
    p_highlighted <- p + geom_highlight(mean(value) > 1, use_direct_label = TRUE)
    expect_equal(p_highlighted$data, d_sieved)
    expect_equal(p_highlighted$layers, list(l_bleached, l_sieved, l_label))
    expect_equal(p_highlighted$guides, list(colour = "none", fill = "none"))
  }
})

test_that("geom_highlight() works the plot with one layer, ungrouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_COLUMN_NAME)

  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = grey07)
  l_sieved <- geom_point(aes(x, y, colour = type), d[d$value > 1, ])

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point()

  expect_equal((p1 + geom_highlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
               list(l_bleached, l_sieved))
})

test_that("geom_highlight() works with two layers, grouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_COLUMN_NAME)

  d_sieved <- d[d$type != "a", ]

  l_bleached_1 <- geom_line(aes_bleached, d_bleached, colour = grey07)
  l_sieved_1 <- geom_line(aes(x, y, colour = type), d_sieved)
  l_bleached_2 <- geom_point(aes_bleached, d_bleached,
                             shape = "circle filled", colour = grey07, fill = grey07)
  l_sieved_2 <- geom_point(aes(x, y, colour = type, fill = type), d_sieved,
                           shape = "circle filled")

  p1 <- ggplot(d, aes(x, y, colour = type, fill = type)) +
    geom_line() +
    geom_point(shape = "circle filled")

  expect_equal((p1 + geom_highlight(mean(value) > 1, use_direct_label = FALSE))$layers,
               list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal((p1 + geom_highlight(mean(value) > 1, n = 1, use_direct_label = FALSE))$layers,
               list(geom_line(), l_bleached_2, l_sieved_2))
})

test_that("geom_highlight() works with two layers, ungrouped", {
  d_bleached <- d
  names(d_bleached)[3] <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
  aes_bleached <- aes(x = x, y = y, colour = NULL, fill = NULL,
                      group = !!VERY_SECRET_COLUMN_NAME)

  d_sieved <- d[d$value > 1, ]

  l_bleached_1 <- geom_point(aes_bleached, d_bleached, shape = "circle open", size = 5,
                             colour = grey07, fill = grey07)
  l_sieved_1 <- geom_point(aes(x, y, colour = type), d_sieved, shape = "circle open", size = 5)
  l_bleached_2 <- geom_point(aes_bleached, d_bleached,
                             colour = grey07, fill = grey07)
  l_sieved_2 <- geom_point(aes(x, y, colour = type), d_sieved)

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point(shape = "circle open", size = 5) +
    geom_point()

  expect_equal((p1 + geom_highlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
               list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal((p1 + geom_highlight(value > 1, n = 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
               list(geom_point(shape = "circle open", size = 5), l_bleached_2, l_sieved_2))
})
