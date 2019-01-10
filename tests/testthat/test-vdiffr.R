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

  # to avoid warnings about old format of grouped_df
  economics_long <- dplyr::group_by(economics_long, add = TRUE)

  expect_doppelganger_if_not_cran(
    "simple line chart",
    ggplot(economics_long, aes(date, value01, colour = variable)) +
      geom_line() +
      gghighlight(mean(value) < 10, label_params = list(seed = 1))
  )

  expect_doppelganger_if_not_cran(
    "simple point chart",
    ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
      geom_point() +
      gghighlight(100 < disp, disp <= 300, use_group_by = FALSE)
  )
})
test_that("gghighlight() highlights sf correctly", {
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  expect_doppelganger_if_not_cran(
    "simple sf map",
    ggplot(nc) +
      geom_sf(aes(fill = AREA)) +
      gghighlight(grepl("C", NAME), use_group_by = FALSE)
  )

})
