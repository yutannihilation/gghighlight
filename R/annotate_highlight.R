#' Annotate Highlightes With Predicates
#'
#' @export
annotate_highlights <- function(.predicate, mapping, grouped = TRUE, ...) {
  predicate <- rlang::enquo(.predicate)
  key <- mapping$label

  if(grouped) {
    filter_func <- build_grouped_filter_func(predicate, key)
  } else {
    filter_func <- build_ungrouped_filter_func(predicate)
  }
  ggrepel::geom_text_repel(mapping, data = filter_func, nudge_x = 45)
}
