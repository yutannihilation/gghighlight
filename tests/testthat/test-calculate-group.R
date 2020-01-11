context("test-calculate-group")


test_that("calculate_group_info() works", {
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

  d_expect <- setNames(d[1:3], c("x", "y", "colour"))
  ids <- c(1, 1, 1, 2, 2, 3, 3)

  expect_equal(calculate_group_info(d, aes(x, y, colour = type)),
               list(data = d_expect, id = ids, key = aes(colour = type)))
  # if there's no discrete key, return NULL
  expect_equal(calculate_group_info(d, aes(x, y, colour = x)),
               NULL)
  # if some aes is the call, caluculated result is used. But it's not used for group keys.
  d_expect_factor <- d_expect
  d_expect_factor$colour <- factor(d_expect_factor$colour)
  expect_equal(calculate_group_info(d, aes(x, y, colour = factor(type))),
               list(data = d_expect_factor, id = ids, key = aes()))

  # if aes contains expressions that cannot be evaluated outside ggplot2 (e.g. after_stat(count)),
  # just ignore it.
  expect_equal(calculate_group_info(d, aes(x, y, colour = type, fill = after_stat(count), alpha = ..count..)),
               list(data = d_expect, id = ids, key = aes(colour = type)))

  # if there is group mapping, use it
  d_expect_group <- d_expect
  d_expect_group$group <- d$type == "a"
  expect_equal(calculate_group_info(d, aes(x, y, group = (type == "a"), colour = type)),
               list(data = d_expect_group, id = c(2, 2, 2, 1, 1, 1, 1), key = aes()))
})


test_that("calculate_group_info() works with facets", {
  d <- tibble::tribble(
    ~idx, ~value, ~cat1, ~cat2,
       1,     10,   "a", "1-2",
       2,     11,   "a", "1-2",
       3,     12,   "a", "3-4",
       4,     13,   "a", "3-4",
       1,      4,   "b", "1-2",
       2,      8,   "b", "1-2",
       3,     16,   "b", "3-4",
       4,     32,   "b", "3-4"
  )

  d_expect <- setNames(d[1:3], c("x", "y", "colour"))
  ids <- c(1, 1, 2, 2, 3, 3, 4, 4)

  expect_equal(calculate_group_info(d, aes(idx, value, colour = cat1), extra_vars = quos(cat2 = cat2)),
               list(data = d_expect, id = ids, key = aes(colour = cat1)))

  # Even if there's no discrete key, return a group info if there's an extra_vars
  expect_equal(calculate_group_info(d, aes(idx, value), extra_vars = quos(cat2 = cat2)),
               list(data = d_expect[, c("x", "y")], id = ids[c(1:4, 1:4)], key = aes()))

  # If extra_vars is empty, it doesn't affect on grouping
  expect_equal(calculate_group_info(d, aes(idx, value), extra_vars = quos()),
               NULL)
  expect_equal(calculate_group_info(d, aes(idx, value, colour = cat1), extra_vars = quos()),
               list(data = d_expect, id = rep(1:2, each = 4), key = aes(colour = cat1)))
})
