context("test-gghighlight.R")

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

d_bleached <- d[1:3]
d_bleached$ids <- factor(ids)
prefix <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
names(d_bleached) <- paste0(prefix, c(1:3, "group"))

aes_bleached <- aes_string(x = paste0(prefix, 1),
                           y = paste0(prefix, 2),
                           colour = paste0(prefix, 3),
                           fill = NULL,
                           group = paste0(prefix, "group"))

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


test_that("gghighlight() does not change the existing layers", {
  p1 <- ggplot(d, aes(x, y, colour = type)) + geom_line()
  p2 <- ggplot(d, aes(x, y, colour = type)) + geom_line()

  invisible(p1 + gghighlight(mean(value) > 1))

  expect_equal(p1, p2)
})

test_that("gghighlight() works the plot with one layer, grouped", {
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
    p_highlighted <- p + gghighlight(mean(value) > 1, use_direct_label = FALSE)
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

  l_label <- ggrepel::geom_label_repel(aes(x, y, colour = type, label = type), d_label, fill = "white")
  for (p in list(p1, p2, p3)) {
    p_highlighted <- p + gghighlight(mean(value) > 1, use_direct_label = TRUE)
    expect_equal(p_highlighted$data, d_sieved)
    expect_equal_layers(p_highlighted$layers, list(l_bleached, l_sieved, l_label))
    expect_equal(p_highlighted$guides, list(colour = "none", fill = "none"))
  }
})

test_that("gghighlight() works the plot with one layer, ungrouped", {
  # Note that the default_aes of fill of point is NA
  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = NA)
  l_sieved <- geom_point(aes(x, y, colour = type), d[d$value > 1, ])

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point()

  expect_equal_layers((p1 + gghighlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
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

test_that("gghighlight() works with two layers, grouped", {
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

  expect_equal_layers((p1 + gghighlight(mean(value) > 1, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal_layers((p1 + gghighlight(mean(value) > 1, n = 1, use_direct_label = FALSE))$layers,
                      list(geom_line(data = d, aes(x, y, colour = type)),
                           l_bleached_2, l_sieved_2))

  # if the data is grouped, the result is the same
  p1$data <- dplyr::group_by(d, .data$type)

  expect_equal_layers((p1 + gghighlight(mean(value) > 1, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n is larger than the number of layers, it throws error.
  expect_error(p1 + gghighlight(mean(value) > 1, n = 3))
})

test_that("gghighlight() works with two layers, ungrouped", {
  d_sieved <- d[d$value > 1, ]

  l_bleached_1 <- geom_point(aes_bleached, d_bleached, shape = "circle open", size = 5,
                             colour = grey07, fill = NA)
  l_sieved_1 <- geom_point(aes(x, y, colour = type), d_sieved, shape = "circle open", size = 5)
  l_bleached_2 <- geom_point(aes_bleached, d_bleached,
                             colour = grey07, fill = NA)
  l_sieved_2 <- geom_point(aes(x, y, colour = type), d_sieved)

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point(shape = "circle open", size = 5) +
    geom_point()

  expect_equal_layers((p1 + gghighlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(l_bleached_1, l_bleached_2, l_sieved_1, l_sieved_2))

  # If n = 1, only one layer above is highlighted.
  expect_equal_layers((p1 + gghighlight(value > 1, n = 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(geom_point(data = d, aes(x, y, colour = type), shape = "circle open", size = 5),
                           l_bleached_2, l_sieved_2))

  # If n is larger than the number of layers, it throws error.
  expect_error(p1 + gghighlight(mean(value) > 1, n = 3))
})

test_that("gghighlight() works with annotations", {
  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = NA)
  l_sieved <- geom_point(aes(x, y, colour = type), d[d$value > 1, ])
  l_annotate <- annotate("text", x = 1, y = 1, label = "foo")

  p <- ggplot(d, aes(x, y, colour = type)) +
    geom_point() +
    l_annotate

  # ignore annotation
  expect_warning(p1 <- p + gghighlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))
  expect_equal_layers(p1$layers,
                      list(l_bleached, l_annotate, l_sieved))

  # raise error
  expect_error(
    suppressWarnings(
      p + gghighlight(no_such_column > 1, use_group_by = FALSE, use_direct_label = FALSE)
    )
  )
})
