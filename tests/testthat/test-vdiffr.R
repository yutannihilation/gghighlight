context("visual tests")

test_that("gghighlight() highlights correctly", {
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

  # to avoid warnings about old format of grouped_df
  economics_long <- dplyr::group_by(economics_long, add = TRUE)

  vdiffr::expect_doppelganger(
    "simple line chart",
    ggplot(economics_long, aes(date, value01, colour = variable)) +
      geom_line() +
      gghighlight(mean(value) < 10, label_params = list(seed = 1))
  )

  vdiffr::expect_doppelganger(
    "simple point chart",
    ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
      geom_point() +
      gghighlight(100 < disp, disp <= 300, use_group_by = FALSE)
  )
})
