#' Highlight Lines With Predicate
#'
#' @inheritDotParams ggplot2::geom_line
#' @export
geom_highlighted_line <- function(.predicate, mapping = NULL,
                                  unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                                  ...) {
  predicate <- rlang::enquo(.predicate)

  key <- if (is.null(mapping$group)) mapping$colour else mapping$group

  if (is.null(key)) stop('Please specify colour or group aesthetics')

  filter_func <- build_grouped_filter_func(predicate, key)

  mapping_orig <- mapping
  mapping$group <- key

  list(
    geom_line(mapping = mapping, colour = unhighlighted_colour),
    geom_line(data = filter_func, mapping_orig, ...)
  )
}
