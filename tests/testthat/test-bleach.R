context("test-bleach")

grey07 <- scales::alpha("grey", 0.7)

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
g_info <- list(data = d_expect, id = ids, key = aes(colour = type))

d_bleached <- d[1:3]
d_bleached$ids <- factor(ids)
prefix <- expr_text(VERY_SECRET_COLUMN_NAME)
names(d_bleached) <- paste0(prefix, c(1:3, "group"))

aes_bleached <- aes_string(x = paste0(prefix, 1),
                           y = paste0(prefix, 2),
                           colour = paste0(prefix, 3),
                           fill = NULL,
                           group = paste0(prefix, "group"))


# tests -------------------------------------------------------------------

test_that("bleach_layer() works", {
  # If colour is specified, colour is used as the group key.
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), g_info, list()),
                     geom_line(aes_bleached, d_bleached, colour = grey07))

  # If colour is specified but group_key is NULL, the result is the same data.
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), NULL, list()),
                     geom_line(aes(colour = NULL, fill = NULL), d, colour = grey07))

  # If the geom accepts fill, it is sets to grey even when it is not included in the mapping.
  expect_equal_layer(bleach_layer(geom_col(aes(colour = type), d), g_info, list()),
                     geom_col(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If the default of colour of the geom is NA and mapping doesn't specify it, params will be NA.
  expect_equal_layer(bleach_layer(geom_col(aes(fill = type), d), g_info, list()),
                     geom_col(aes_bleached, d_bleached, colour = NA, fill = grey07))

  # If colour and fill is specified at the same time, fill is used as the group key.
  expect_equal_layer(bleach_layer(geom_col(aes(colour = type, fill = type), d), g_info, list()),
                     geom_col(aes_bleached, d_bleached, colour = grey07, fill = grey07))

  # If mapping doesn't have colour or fill, group or x aes can be used as group key.
  # c.f. https://github.com/yutannihilation/gghighlight/pull/17#issuecomment-390486101.
  expect_equal_layer(bleach_layer(geom_col(aes(group = type), d), g_info, list()),
                     geom_col(aes_bleached, d_bleached, colour = NA, fill = grey07))
  expect_equal_layer(bleach_layer(geom_col(aes(x = type), d), g_info, list()),
                     geom_col(aes_bleached, d_bleached, colour = NA, fill = grey07))

  # unhighlighted_params can be more detailed
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), g_info,
                                  list(colour = "blue", size = 3)),
                     geom_line(aes_bleached, d_bleached, colour = "blue", size = 3))

  expect_equal_layer(bleach_layer(geom_col(aes(colour = type, fill = type), d), g_info,
                                  list(colour = "blue", width = 0.5)),
                     geom_col(aes_bleached, d_bleached, colour = "blue", fill = grey07, width = 0.5))

  expect_equal_layer(bleach_layer(geom_col(aes(fill = type), d), g_info,
                                  list(fill = "blue", width = 0.5)),
                     # TODO: the order of fill and colour matters here...
                     geom_col(aes_bleached, d_bleached, fill = "blue", colour = NA, width = 0.5))

  expect_equal_layer(bleach_layer(geom_col(aes(colour = type, fill = type), d), g_info,
                                  list(colour = scales::alpha("grey", 0.9), fill = grey07, width = 0.5)),
                     geom_col(aes_bleached, d_bleached, colour = scales::alpha("grey", 0.9), fill = grey07, width = 0.5))
})

test_that("bleach_layer() works for NULL default aes", {
  skip_if_not_installed("sf")
  
  expect_equal_layer(
    bleach_layer(geom_sf(aes(fill = type), d)[[1]],
                 list(data = setNames(d[3], c("fill")),
                      id = ids,
                      key = aes(fill = type)),
                 list()),
    geom_sf(aes_string(fill = paste0(prefix, 1),
                       colour = NULL,
                       group = paste0(prefix, "group")),
            setNames(d_bleached[, 3:4], paste0(prefix, c("1", "group"))),
            colour = grey07,
            fill = grey07)[[1]]
  )

})

test_that("bleach_layer(use_facet_vars = TRUE) adds the original data", {
  # If colour is specified, colour is used as the group key.
  expect_equal_layer(bleach_layer(geom_line(aes(colour = type), d), g_info, list(), use_facet_vars = TRUE),
                     geom_line(aes_bleached, dplyr::bind_cols(d_bleached, d), colour = grey07))

})
