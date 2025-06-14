test_that("gghighlight() highlights correctly", {
  testthat::skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger(
    "simple bar chart",
    ggplot(mpg, aes(class, fill = factor(cyl))) +
      geom_bar() +
      gghighlight(cyl >= 8, use_group_by = FALSE)
  )

  vdiffr::expect_doppelganger(
    "simple bar chart with facet",
    ggplot(mpg, aes(class, fill = factor(cyl))) +
      geom_bar() +
      gghighlight() +
      facet_wrap(vars(cyl))
  )

  vdiffr::expect_doppelganger(
    "simple point chart",
    ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
      geom_point() +
      gghighlight(100 < disp, disp <= 300, use_group_by = FALSE)
  )

  set.seed(99)
  value <- unlist(lapply(c(1, 10, 20), function(x) {
    cumsum(runif(100, min = -x, max = x))
  }))
  d_line <- data.frame(
    index = rep(1:100, times = 3),
    value = value,
    value01 = value / rep(c(1, 10, 20), each = 100),
    group = rep(c("a", "b", "c"), each = 100),
    stringsAsFactors = FALSE
  )

  vdiffr::expect_doppelganger(
    "simple line chart",
    ggplot(d_line, aes(index, value01, colour = group)) +
      geom_line() +
      gghighlight(mean(value) < 10, label_params = list(seed = 1))
  )
})
