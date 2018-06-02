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

d_ <- setNames(d[1:3], c("x", "y", "colour"))
ids <- c(1, 1, 1, 2, 2, 3, 3)
g_info <- list(data = d_, id = ids, key = aes(colour = type))

expect_equal_layer <- function(x, y) {
  x$mapping <- x$mapping[sort(names(x$mapping))]
  y$mapping <- x$mapping[sort(names(y$mapping))]
  expect_equal(x, y)
}

expect_equal_layers <- function(x, y) {
  purrr::walk2(x, y, expect_equal_layer)
}

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

  # Aesthetics that are required by stat also stays in mapping (Note that the orders are different)
  m <- merge_mapping(geom_boxplot(aes(x, y, colour = b)), NULL)
  expect_equal(m, aes(x, y, colour = b)[names(m)])
})

test_that("merge_data() works", {
  # if oneare NULL, return the other one as is
  expect_equal(merge_data(geom_bar(data = d), waiver()),
               d)
  expect_equal(merge_data(geom_bar(), d),
               d)
})

test_that("calculate_group_info() works", {
  expect_equal(calculate_group_info(d, aes(x, y, colour = type)),
               list(data = setNames(d[1:3], c("x", "y", "colour")), id = ids, key = aes(colour = type)))
  # if there's no discrete key, return NULL
  expect_equal(calculate_group_info(d, aes(x, y, colour = x)),
               NULL)
  # if some aes is the call, caluculated result is used. But it's not used for group keys.
  d2 <- setNames(d[1:3], c("x", "y", "colour"))
  d2$colour <- factor(d2$colour)
  expect_equal(calculate_group_info(d, aes(x, y, colour = factor(type))),
               list(data = d2, id = ids, key = aes()))
})


d_bleached <- d[1:3]
d_bleached$ids <- factor(ids)
prefix <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
names(d_bleached) <- paste0(prefix, c(1:3, "group"))

aes_bleached <- aes_string(x = paste0(prefix, 1),
                           y = paste0(prefix, 2),
                           colour = paste0(prefix, 3),
                           fill = NULL,
                           group = paste0(prefix, "group"))

test_that("bleach_layer() works", {
  # If colour is specified, colour is used as the group key.
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), g_info, grey07),
                     geom_line(aes_bleached, d_bleached, colour = grey07))

  # If colour is specified but group_key is NULL, the result is the same data.
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), NULL, grey07),
                     geom_line(aes(colour = NULL, fill = NULL), d, colour = grey07))

  # If the geom accepts fill, it is sets to grey even when it is not included in the mapping.
  expect_equal_layer(bleach_layer(geom_bar(aes(colour = type), d), g_info, grey07),
                     geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If colour and fill is specified at the same time, fill is used as the group key.
  expect_equal_layer(bleach_layer(geom_bar(aes(colour = type, fill = type), d), g_info, grey07),
                     geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If mapping doesn't have colour or fill, group or x aes can be used as group key.
  # c.f. https://github.com/yutannihilation/gghighlight/pull/17#issuecomment-390486101.
  expect_equal_layer(bleach_layer(geom_bar(aes(group = type), d), g_info, grey07),
                     geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))
  expect_equal_layer(bleach_layer(geom_bar(aes(x = type), d), g_info, grey07),
                     geom_bar(aes_bleached, d_bleached, colour = grey07, fill = grey07))
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
  f <- function(...) sieve_layer(geom_bar(aes(colour = type), d), g_info, pred_grouped, ...)
  # Basic usage.
  expect_equal(f(), geom_bar(aes(colour = type), d_sieved_grouped))
  # Large number of max_highlights doesn't affect the result.
  expect_equal(f(max_highlight = 100L), geom_bar(aes(colour = type), d_sieved_grouped))
  # Even when the max_highlight is smaller, if no numeric predicates are available,
  # the result is not sliced down to the number.
  expect_equal(f(max_highlight = 1L), geom_bar(aes(colour = type), d[d$type != "a", ]))
  # When predicate is numerical, the result is sliced.
  expect_equal(sieve_layer(geom_bar(aes(colour = type), d), g_info,
                           list(rlang::quo(mean(value))), max_highlight = 1L),
               geom_bar(aes(colour = type), d[6:7, ]))

  # can be grouped, but intentionally avoid group_by;
  # the result is same no matter group_key is provided or not
  f <- function (key, use_group_by) {
    info <- g_info
    info$key <- key
    sieve_layer(geom_bar(aes(colour = type), d), info, pred_ungrouped, use_group_by = use_group_by)
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
  l <- sieve_layer(geom_bar(aes(colour = type), d), g_info, pred_use_group_var)
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

  d2_ <- setNames(d2[1], c("colour"))
  ids2 <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
  g_info2 <- list(data = d2_, id = ids2, key = aes(colour = type))


  # logical predicates only; max_highlight is ignored.
  expect_equal(sieve_layer(geom_line(aes(colour = type), d2), g_info2,
                           pred_grouped[1:2], max_highlight = 2),
               geom_line(aes(colour = type), d2[!d2$type %in% c("a", "d"), ]))

  expect_equal(sieve_layer(geom_line(aes(colour = type), d2), g_info2,
                           pred_grouped, max_highlight = 2),
               geom_line(aes(colour = type), d2[c(3,4,9,10), ]))
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
  sl <- sieve_layer(geom_bar(aes(x), d3), NULL, rlang::quos(p1 = l, p2 = v), max_highlight = 2)
  expect_identical(sl$mapping, aes(x))
  expect_identical(sl$data, d3[3:4, ])

  # grouped
  d3_ <- setNames(d3[3], c("colour"))
  ids3 <- c(1, 1, 2, 2)
  group_info3 <- list(data = d3_, id = ids3, key = aes(colour = z))
  sl <- sieve_layer(geom_line(aes(x, v, colour = z), d3), group_info3,
                    rlang::quos(p1 = list(l), p2 = sum(v)), max_highlight = 1, use_group_by = TRUE)
  expect_identical(sl$mapping, aes(x, v, colour = z))
  expect_identical(sl$data, d3[3:4, ])
})

test_that("geom_highlight() does not change the existing layers", {
  p1 <- ggplot(d, aes(x, y, colour = type)) + geom_line()
  p2 <- ggplot(d, aes(x, y, colour = type)) + geom_line()

  invisible(p1 + geom_highlight(mean(value) > 1))

  expect_equal(p1, p2)
})

test_that("geom_highlight() works the plot with one layer, grouped", {
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
    expect_equal_layers(p_highlighted$layers, list(l_bleached, l_sieved))
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
    expect_equal_layers(p_highlighted$layers, list(l_bleached, l_sieved, l_label))
    expect_equal(p_highlighted$guides, list(colour = "none", fill = "none"))
  }
})

test_that("geom_highlight() works the plot with one layer, ungrouped", {
  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = grey07)
  l_sieved <- geom_point(aes(x, y, colour = type), d[d$value > 1, ])

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point()

  expect_equal_layers((p1 + geom_highlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(l_bleached, l_sieved))
})


d_bleached <- d[1:3]
d_bleached$ids <- factor(ids)
prefix <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
names(d_bleached) <- paste0(prefix, c(1:3, "group"))

aes_bleached <- aes_string(x = paste0(prefix, 1),
                           y = paste0(prefix, 2),
                           colour = paste0(prefix, 3),
                           fill = NULL,
                           group = paste0(prefix, "group"))

test_that("geom_highlight() works with two layers, grouped", {
  aes_bleached2 <- aes_bleached
  aes_bleached2$fill <- rlang::quo(!!rlang::sym(paste0(prefix, 4)))
  d_bleached2 <- d_bleached
  d_bleached2[paste0(prefix, 4)] <- d[3]

  d_sieved <- d[d$type != "a", ]

  l_bleached_1 <- geom_line(aes_bleached, d_bleached, colour = grey07)
  l_sieved_1 <- geom_line(aes(x, y, colour = type), d_sieved)
  l_bleached_2 <- geom_point(aes_bleached2, d_bleached2,
                             shape = "circle filled", colour = grey07, fill = grey07)
  l_sieved_2 <- geom_point(aes(x, y, colour = type, fill = type), d_sieved,
                           shape = "circle filled")

  p1 <- ggplot(d, aes(x, y, colour = type, fill = type)) +
    geom_line() +
    geom_point(shape = "circle filled")

  expect_equal_layers((p1 + geom_highlight(mean(value) > 1, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal_layers((p1 + geom_highlight(mean(value) > 1, n = 1, use_direct_label = FALSE))$layers,
                      list(geom_line(data = d, aes(x, y, colour = type)),
                           l_bleached_2, l_sieved_2))

  # if the data is grouped, the result is the same
  p1$data <- dplyr::group_by(d, .data$type)

  expect_equal_layers((p1 + geom_highlight(mean(value) > 1, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n is larger than the number of layers, it throws error.
  expect_error(p1 + geom_highlight(mean(value) > 1, n = 3))
})

test_that("geom_highlight() works with two layers, ungrouped", {
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

  expect_equal_layers((p1 + geom_highlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal_layers((p1 + geom_highlight(value > 1, n = 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(geom_point(data = d, aes(x, y, colour = type), shape = "circle open", size = 5),
                           l_bleached_2, l_sieved_2))

  # If n is larger than the number of layers, it throws error.
  expect_error(p1 + geom_highlight(mean(value) > 1, n = 3))
})
