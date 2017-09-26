#' Highlight Lines With Predicate
#'
#' @inheritDotParams ggplot2::geom_line
#' @export
geom_highlighted_line <- function(.predicate,
                                  mapping = NULL,
                                  unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                                  .group_key = NULL,
                                  ...) {
  predicate <- rlang::enquo(.predicate)

  key <- rlang::enquo(.group_key)
  # We cannot use is.null directly on .group_key;
  # It fails if some name is specified because the name only makes sense in the layer data.
  if (rlang::quo_is_null(key)) {
    key <- mapping$group %||% mapping$colour
  }
  if (is.null(key)) {
    warning('Specify .group_key or group aes if you want to filter the data by group.')
  }

  filter_func <- build_grouped_filter_func(predicate, key)

  mapping_unhighlitghted <- mapping %||% aes()
  mapping_unhighlitghted$group <- rlang::quo_expr(key)

  list(
    ggplot2::geom_line(mapping = mapping_unhighlitghted, colour = unhighlighted_colour, ...),
    ggplot2::geom_line(mapping = mapping, data = filter_func, ...)
  )
}
