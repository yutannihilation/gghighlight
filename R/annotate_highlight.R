#' Annotate Highlightes With Predicates
#'
#' @export
annotate_highlights <- function(.predicate, mapping, grouped = TRUE, ...) {
  predicate <- rlang::enquo(.predicate)
  key <- mapping$label

  filter_func <- build_filter_func(predicate, key)
  ggrepel::geom_text_repel(mapping, data = filter_func, nudge_x = 45)
}

build_filter_func <- function(predicate, key = NULL) {
  function(df) {
    gdf <- if(is.null(key)) df else dplyr::group_by(df, !! key)
    fdf <- dplyr::filter(gdf, !! predicate)
    fdf <- dplyr::summarise_all(fdf, dplyr::last)
    dplyr::ungroup(fdf)
  }
}
