#' Highlight Lines With Predicate
#'
#' @export
geom_highlighted_line <- function(.predicate, mapping, ...) {
  predicate <- rlang::enquo(.predicate)
  key <- mapping$colour

  if(is.null(key)) stop('Please specify colour aesthetics')

  filter_func <- build_grouped_filter_func(predicate, key)

  # overwrite aesthetic mappings
  if(!is.null(mapping$group)) warning('group aesthetics is overwritten.')
  mapping$group  <- rlang::sym('.group')
  mapping$alpha  <- rlang::sym('.alpha')

  geom_line(data = filter_func, mapping, ...)
}
