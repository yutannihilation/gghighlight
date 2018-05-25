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

test_that("choose_layer_for_label() chooses a layer properly", {
  d <- data.frame(x = 1, y = 2, type = "a")
  d_wo_labellables <- data.frame(x = 1, y = 2)

  type_quo <- rlang::quo(type)
  
  l_point <- geom_point(aes(x, y, colour = type), d)
  l_point_no_type_in_aes <- geom_point(aes(x, y), d)
  l_point_no_type_in_data <- geom_point(aes(x, y, colour = x), d_wo_labellables)

  # if label_key is specified and the layer contains it, it should be choosed
  expect_equal(choose_layer_for_label(list(l_point), list(type_quo), type_quo),
               list(layer = l_point, group_key = type_quo, label_key = type_quo))
  expect_equal(choose_layer_for_label(list(l_point), list(NULL), type_quo),
               list(layer = l_point, group_key = NULL, label_key = type_quo))
})
