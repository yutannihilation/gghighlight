context("test-bad-arguments")

test_that("mistype of `==` is detected", {
  expect <- "Did you mistype some argument name? Or, did you mean `==`?: a = b"
  expect_error(check_bad_predicates(rlang::quos(a = b)), expect, fixed = TRUE)
  expect_error(gghighlight(a = b), expect, fixed = TRUE)
})

test_that("label_key must be a symbol", {
  expect_error(check_bad_label_key(rlang::quo("foo")), "label_key must be a symbol", fixed = TRUE)
  expect_error(gghighlight(label_key = "foo"), "label_key must be a symbol", fixed = TRUE)
  foo <- rlang::quo(foo)
  expect_error(gghighlight(label_key = !!foo), NA)
})
