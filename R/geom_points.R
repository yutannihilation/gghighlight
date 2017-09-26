#' Highlight Points With Predicate
#'
#' @inheritDotParams ggplot2::geom_point
#' @export
geom_highlighted_point <- function(.predicate, mapping = NULL,
                                   unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                                   ...) {
  predicate <- rlang::enquo(.predicate)

  filter_func <- build_ungrouped_filter_func(predicate, key)

  list(
    geom_point(mapping = mapping, colour = unhighlighted_colour, ...),
    geom_point(data = filter_func, mapping_orig, ...)
  )
}

