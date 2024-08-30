grey07 <- "#BEBEBEB2"

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

aes_bleached <- aes(
  x      = !!sym(paste0(prefix, "1")),
  y      = !!sym(paste0(prefix, "2")),
  colour = !!sym(paste0(prefix, "3")),
  fill   = NULL,
  group  = !!sym(paste0(prefix, "group"))
)


# tests -------------------------------------------------------------------


test_that("gghighlight() does not change the existing layers", {
  p1 <- ggplot(d, aes(x, y, colour = type)) + geom_line()
  p2 <- ggplot(d, aes(x, y, colour = type)) + geom_line()

  invisible(p1 + gghighlight(mean(value) > 1))

  expect_equal(p1$data, d)
  expect_equal(length(p1$layers), 1L)
  expect_equal(as.list(p1$layers[[1]]), as.list(p2$layers[[1]]))
})

test_that("gghighlight() renames layer name on ggplot2", {
  p <- ggplot(d, aes(x, y, colour = type)) + geom_line(name = "foo") + gghighlight()

  expect_equal(
    names(p$layers),
    c(
      "foo",          # First layer uses the same name
      "foo__sieved",  # Additional layer has a suffix to avoid duplicated name
      "geom"          # Layers for label uses the default name.
    )
  )
})

test_that("gghighlight() works with the plot with one layer, grouped", {
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
    if (utils::packageVersion("ggplot2") < "3.5.0") {
      expect_equal(p_highlighted$guides, NULL)
    } else {
      expect_equal(p_highlighted$guides$guides, NULL)
    }
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
    if (utils::packageVersion("ggplot2") < "3.5.0") {
      expect_equal(p_highlighted$guides, list(colour = "none", fill = "none"))
    } else {
      expect_equal(p_highlighted$guides$guides, list(colour = "none", fill = "none"))
    }
  }
})

test_that("gghighlight() works with the plot without layers, grouped", {
  d_sieved <- d[d$type != "a", ]

  p <- ggplot(d, aes(x, y, colour = type)) + gghighlight(mean(value) > 1, use_direct_label = FALSE)
  expect_equal(p$data, d_sieved)
})

test_that("gghighlight() works with the plot with one layer, ungrouped", {
  # Note that the default_aes of fill of point is NA
  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = NA)
  l_sieved <- geom_point(aes(x, y, colour = type), d[d$value > 1, ])

  p1 <- ggplot(d, aes(x, y, colour = type)) +
    geom_point()

  expect_equal_layers((p1 + gghighlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE))$layers,
                      list(l_bleached, l_sieved))
})

test_that("gghighlight() works with the plot without layers, grouped", {
  d_sieved <- d[d$value > 1, ]

  p <- ggplot(d, aes(x, y, colour = type)) + gghighlight(value > 1, use_group_by = FALSE, use_direct_label = FALSE)
  expect_equal(p$data, d_sieved)
})



d_bleached <- d[1:3]
d_bleached$ids <- factor(ids)
prefix <- expr_text(VERY_SECRET_COLUMN_NAME)
names(d_bleached) <- paste0(prefix, c(1:3, "group"))

aes_bleached <- aes(
  x      = !!sym(paste0(prefix, "1")),
  y      = !!sym(paste0(prefix, "2")),
  colour = !!sym(paste0(prefix, "3")),
  fill   = NULL,
  group  = !!sym(paste0(prefix, "group"))
)

test_that("gghighlight() works with two layers, grouped", {
  aes_bleached2 <- aes_bleached
  aes_bleached2$fill <- quo(!!sym(paste0(prefix, 4)))
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

test_that("gghighlight() works with facets", {
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
  g_info <- list(data = d_expect, id = ids, key = aes(colour = cat1))

  d_bleached <- d[1:3]
  d_bleached$ids <- factor(ids)
  prefix <- expr_text(VERY_SECRET_COLUMN_NAME)
  names(d_bleached) <- paste0(prefix, c(1:3, "group"))
  d_bleached <- dplyr::bind_cols(d_bleached, d)

  aes_bleached <- aes(
    x      = !!sym(paste0(prefix, "1")),
    y      = !!sym(paste0(prefix, "2")),
    colour = !!sym(paste0(prefix, "3")),
    fill   = NULL,
    group  = !!sym(paste0(prefix, "group"))
  )

  l_bleached <- geom_point(aes_bleached, d_bleached, colour = grey07, fill = NA)
  l_sieved <- geom_point(aes(idx, value, colour = cat1), d[-(5:6), ])

  # grouped
  p1 <- ggplot(d, aes(idx, value, colour = cat1)) +
    geom_point() +
    facet_wrap(vars(cat2)) +
    gghighlight(max(value) > 10, use_group_by = TRUE, use_direct_label = FALSE, calculate_per_facet = TRUE)

  expect_equal_layers(p1$layers, list(l_bleached, l_sieved))

  # ungrouped
  p2 <- ggplot(d, aes(idx, value, colour = cat1)) +
    geom_point() +
    facet_wrap(vars(cat2)) +
    gghighlight(value > 10, use_group_by = FALSE, use_direct_label = FALSE, calculate_per_facet = TRUE)

  l_sieved2 <- geom_point(aes(idx, value, colour = cat1), d[d$value > 10, ])
  expect_equal_layers(p2$layers, list(l_bleached, l_sieved2))
})
