type_chr <- c("a", "a", "b", "b")
d <- data.frame(x = 1:4, y = 1:4, type = type_chr, type2 = type_chr, stringsAsFactors = FALSE)
d2 <- data.frame(x = 1, y = 2, type = "a")

d_ <- setNames(d[1:3], c("x", "y", "colour"))
ids <- c(1, 1, 2, 2)

l_point <- geom_point(aes(x, y, colour = type), d)
l_point2 <- geom_point(aes(x, y, colour = type), d, shape = "filled circle")
l_line <- geom_line(aes(x, y, colour = type), d)
l_bar <- geom_bar(aes(x, fill = type), d)
l_text <- geom_text(aes(x, y, label = type), d)

type_quo <- quo(type)
type2_quo <- quo(type2)
null_quo <- quo(NULL)

g_info <- list(data = d_, id = ids, key = aes(colour = type))


test_that("The types of layers are detected properly", {
  expect_true(is_identity_line(geom_line()))
  expect_false(is_identity_line(geom_bar()))
  expect_false(is_identity_line(geom_line(stat = "bin")))

  expect_true(is_identity_point(geom_point()))
  expect_false(is_identity_point(geom_bar()))
  expect_false(is_identity_point(geom_point(stat = "bin")))

  expect_true(is_bar(geom_bar()))
  expect_true(is_bar(geom_histogram()))
  expect_false(is_bar(geom_point()))
})

test_that("choose_layer_for_label() chooses a layer properly", {
  # if label_key is specified and the layer contains it, it should be choosed
  expect_equal(choose_layer_for_label(list(l_point), list(g_info), type2_quo),
               list(layer = l_point, label_key = type2_quo))
  expect_equal(choose_layer_for_label(list(l_line), list(g_info), type2_quo),
               list(layer = l_line, label_key = type2_quo))
  expect_equal(choose_layer_for_label(list(l_bar), list(g_info), type2_quo),
               list(layer = l_bar, label_key = type2_quo))
  # If label_key is provided explicitly it's OK that group_key is NULL.
  expect_equal(choose_layer_for_label(list(l_point), NULL, type2_quo),
               list(layer = l_point, label_key = type2_quo))
  # If label_key is not provided, guess from the group_info.
  expect_equal(choose_layer_for_label(list(l_point), list(g_info), null_quo),
               list(layer = l_point, label_key = type_quo))
  # If both is NULL, no layer is chosen.
  expect_equal(choose_layer_for_label(list(l_point), NULL, null_quo),
               NULL)
  # If there are two layers, the first one is chosen.
  expect_equal(choose_layer_for_label(list(l_point, l_point2), list(g_info, g_info), type2_quo),
               list(layer = l_point, label_key = type2_quo))
  # line > point > bar
  expect_equal(choose_layer_for_label(list(l_point, l_line), list(g_info, g_info), type2_quo),
               list(layer = l_line, label_key = type2_quo))
  expect_equal(choose_layer_for_label(list(l_bar, l_point), list(g_info, g_info), type2_quo),
               list(layer = l_point, label_key = type2_quo))
  # If there are two layers and one is an unsupported geom, the other one is returned.
  expect_equal(choose_layer_for_label(list(l_text, l_point), list(g_info, g_info), type2_quo),
               list(layer = l_point, label_key = type2_quo))
  # if label_key is specified but no layer contains it, NULL is returned.
  expect_equal(choose_layer_for_label(list(l_text, l_point), list(g_info, g_ingo), quo(no_such_column)),
               NULL)
})

test_that("generate_labelled_layer() geenrates a layer for label.", {
  expect_equal(generate_labelled_layer(list(l_point), list(g_info), type2_quo, list(fill = "white"), Inf),
               ggrepel::geom_label_repel(aes(x, y, colour = type, label = type2), d, fill = "white"))
  # it accepts call
  expect_equal(generate_labelled_layer(list(l_point), list(g_info), quo(factor(type2)), list(fill = "white"), Inf),
               ggrepel::geom_label_repel(aes(x, y, colour = type, label = factor(type2)), d, fill = "white"))

  expect_equal(generate_labelled_layer(list(l_point), list(g_info), quo(no_such_column), list(fill = "white"), Inf),
               NULL)
  expect_equal(generate_labelled_layer(list(l_line), list(g_info), type2_quo, list(fill = "white"), Inf),
               ggrepel::geom_label_repel(aes(x, y, colour = type, label = type2), d[c(2, 4), ], fill = "white"))
  expect_equal(generate_labelled_layer(list(l_bar), list(g_info), type2_quo, list(fill = "white"), Inf),
               list())
  # Do not generate labels when the data is more than max_labels
  expect_equal(generate_labelled_layer(list(l_point), list(g_info), type2_quo, list(fill = "white"), 1),
               list())
  expect_equal(generate_labelled_layer(list(l_line), list(g_info), type2_quo, list(fill = "white"), 1),
               list())
  # share the same seed of jitter
  l_jitter <- geom_point(aes(x, y, colour = type), d, position = position_jitter())
  expect_equal(generate_labelled_layer(list(l_jitter), list(g_info), type2_quo, list(fill = "white"), Inf),
               ggrepel::geom_label_repel(aes(x, y, colour = type, label = type2), d, fill = "white",
                                         position = position_jitter(seed = l_jitter$position$seed)))

  # when the seed is NULL, set it and share it. (c.f. tidyverse/ggplot2#2507)
  l_jitter <- geom_point(aes(x, y, colour = type), d, position = "jitter")
  expect_true(is.null(l_jitter$position$seed))
  l_label <- generate_labelled_layer(list(l_jitter), list(g_info), type2_quo, list(fill = "white"), Inf)
  expect_true(!is.null(l_jitter$position$seed))
  expect_equal(l_label$position$seed, l_jitter$position$seed)
})

test_that("call_ggrepel_with_params() generates a geom_label_repel()", {
  expect_equal(
    call_ggrepel_with_params(aes(x, y, colour = type, label = type), d, list(fill = "white")),
    ggrepel::geom_label_repel(aes(x, y, colour = type, label = type), d, fill = "white")
  )
})
