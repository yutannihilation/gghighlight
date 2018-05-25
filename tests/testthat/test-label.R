context("test-label.R")

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

test_that("infer_label_key() infers the label key properly", {
  d <- data.frame(x = 1, y = 2, type = "a", type2 = "b", stringsAsFactors = FALSE)
  d2 <- data.frame(x = 1, y = 2, type = "a")
  type_quo <- rlang::quo(type)
  type2_quo <- rlang::quo(type2)

  # If no group_key is available, use the first labellable column.
  expect_equal(infer_label_key(geom_point(aes(x, y, colour = type), d), NULL),
               type_quo)
  # factor is also labellable.
  expect_equal(infer_label_key(geom_point(aes(x, y, colour = type), d2), NULL),
               type_quo)
  # If group_key is supplied, use it.
  expect_equal(infer_label_key(geom_point(aes(x, y, colour = type), d), type2_quo),
               type2_quo)
  # If data does not contain character or factor columns, return NULL
  expect_equal(infer_label_key(geom_point(aes(x, y, colour = type), d[, c("x", "y")]), NULL),
               NULL)
})

test_that("choose_layer_for_label() chooses a layer properly", {
  d <- data.frame(x = 1, y = 2, type = "a")
  d_wo_labellables <- data.frame(x = 1, y = 2)

  type_quo <- rlang::quo(type)
  null_quo <- rlang::quo(NULL)

  l_point <- geom_point(aes(x, y, colour = type), d)
  l_point2 <- geom_point(aes(x, y, colour = type), d, shape = "filled circle")
  l_line <- geom_line(aes(x, y, colour = type), d)
  l_bar <- geom_bar(aes(x, fill = type), d)
  l_point_no_type_in_aes <- geom_point(aes(x, y), d)
  l_point_no_type_in_data <- geom_point(aes(x, y, colour = x), d_wo_labellables)
  l_text <- geom_text(aes(x, y, label = type), d)

  # if label_key is specified and the layer contains it, it should be choosed
  expect_equal(choose_layer_for_label(list(l_point), list(type_quo), type_quo),
               list(layer = l_point, label_key = type_quo))
  expect_equal(choose_layer_for_label(list(l_line), list(type_quo), type_quo),
               list(layer = l_line, label_key = type_quo))
  expect_equal(choose_layer_for_label(list(l_bar), list(type_quo), type_quo),
               list(layer = l_bar, label_key = type_quo))
  # If group_key is NULL, it's OK.
  expect_equal(choose_layer_for_label(list(l_point), list(NULL), type_quo),
               list(layer = l_point, label_key = type_quo))
  # If type_key is NULL, it's OK.
  expect_equal(choose_layer_for_label(list(l_point), list(type_quo), null_quo),
               list(layer = l_point, label_key = type_quo))
  # If both is NULL, label_key is NULL
  expect_equal(choose_layer_for_label(list(l_point), list(NULL), null_quo),
               list(layer = l_point, label_key = type_quo))
  # If there are two layers, the first one is chosen
  expect_equal(choose_layer_for_label(list(l_point, l_point2), list(type_quo, type_quo), type_quo),
               list(layer = l_point, label_key = type_quo))
  # line > point > bar
  expect_equal(choose_layer_for_label(list(l_point, l_line), list(type_quo, type_quo), type_quo),
               list(layer = l_line, label_key = type_quo))
  expect_equal(choose_layer_for_label(list(l_bar, l_point), list(type_quo, type_quo), type_quo),
               list(layer = l_point, label_key = type_quo))
  # If there are two layers and one is an unsupported geom, the other one is returned.
  expect_equal(choose_layer_for_label(list(l_text, l_point), list(type_quo, type_quo), type_quo),
               list(layer = l_point, label_key = type_quo))
  # if label_key is not specified but no layer contains it, NULL is returned.
  expect_equal(choose_layer_for_label(list(l_text, l_point), list(type_quo, type_quo), rlang::quo(no_such_column)),
               NULL)
})

test_that("generate_labelled_layer() geenrates a layer for label.", {
  d <- data.frame(x = 1, y = 2, type = "a")
  type_quo <- rlang::quo(type)
  
  expect_equal(generate_labelled_layer(list(geom_point(aes(x, y, colour = type), d)), list(type_quo), type_quo),
               ggrepel::geom_label_repel(aes(x, y, colour = type, label = type), d))
  expect_equal(generate_labelled_layer(list(geom_point(aes(x, y, colour = type), d)), list(type_quo), rlang::quo(no_such_column)),
               NULL)
})
