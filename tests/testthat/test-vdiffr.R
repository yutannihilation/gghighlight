context("visual tests")

test_that("gghighlight() highlights correctly", {
  expect_doppelganger_if_not_cran(
    "simple bar chart",
    ggplot(mpg, aes(class, fill = factor(cyl))) +
      geom_bar() +
      gghighlight(cyl >= 8, use_group_by = FALSE)
  )

  expect_doppelganger_if_not_cran(
    "simple bar chart with facet",
    ggplot(mpg, aes(class, fill = factor(cyl))) +
      geom_bar() +
      gghighlight() +
      facet_wrap(vars(cyl))
  )
})
