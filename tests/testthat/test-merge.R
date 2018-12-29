context("test-merge")

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

# test others -------------------------------------------------------------


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
  expect_equal(merge_data(geom_bar(data = d), waiver()), d)
  expect_equal(merge_data(geom_bar(), d),                d)
})
