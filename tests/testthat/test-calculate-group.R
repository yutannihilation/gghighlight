context("test-calculate-group")

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

  # if aes contains expressions that cannot be evaluated outside ggplot2 (e.g. stat(count)),
  # just ignore it.
  expect_equal(calculate_group_info(d, aes(x, y, colour = type, fill = stat(count), alpha = ..count..)),
               list(data = setNames(d[1:3], c("x", "y", "colour")), id = ids, key = aes(colour = type)))

  # if there is group mapping, use it
  d_with_group <- setNames(d[1:3], c("x", "y", "colour"))
  d_with_group$group <- d$type == "a"
  expect_equal(calculate_group_info(d, aes(x, y, group = (type == "a"), colour = type)),
               list(data = d_with_group, id = c(2, 2, 2, 1, 1, 1, 1), key = aes()))
})
