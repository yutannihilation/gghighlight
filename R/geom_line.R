#' Highlight Lines With Predicate
#'
#' @export
geom_highlighted_line <- function(.predicate, mapping = NULL,
                                  unhighlighted_colour = ggplot2::alpha("grey", 0.3), ...) {
  predicate <- rlang::enquo(.predicate)
  key <- mapping$colour

  if(is.null(key)) stop('Please specify colour aesthetics')

  filter_func <- build_grouped_filter_func(predicate, key)

  mapping_orig <- mapping
  mapping$group <- mapping$group %||% mapping$colour

  list(
    geom_line(mapping = mapping, colour = unhighlighted_colour),
    geom_line(data = filter_func, mapping_orig, ...)
  )
}
