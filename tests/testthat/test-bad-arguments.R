context("test-bad-arguments")

test_that("mistype of `==` is detected", {
  expect_error(check_bad_predicates(rlang::quos(a = b)), "Did you mistyped some argument name? Or, you meant `==`?: a = b", fixed = TRUE)
  expect_error(gghighlight(a = b), "Did you mistyped some argument name? Or, you meant `==`?: a = b", fixed = TRUE)
})

test_that("label_key must be a symbol", {
  expect_error(check_bad_label_key(rlang::quo("foo")), "label_key must be a symbol", fixed = TRUE)
  expect_error(gghighlight(label_key = "foo"), "label_key must be a symbol", fixed = TRUE)
  foo <- rlang::quo(foo)
  expect_error(gghighlight(label_key = !!foo), NA)
})
