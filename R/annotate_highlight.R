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

build_grouped_filter_func <- function(predicate, key = NULL) {
  function(df) {
    gdf <- if(is.null(key)) df else dplyr::group_by(df, !! key)
    fdf <- dplyr::filter(gdf, !! predicate)
    # TODO: provide a way to customize how to choose representions
    fdf <- dplyr::summarise_all(fdf, dplyr::last)
    dplyr::ungroup(fdf)
  }
}

build_ungrouped_filter_func <- function(predicate) {
  function(df) {
    fdf <- dplyr::filter(df, !! predicate)
    dplyr::ungroup(fdf)
  }
}
