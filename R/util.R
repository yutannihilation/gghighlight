# Utilities

build_grouped_filter_func <- function(predicate, key) {
  function(df) {
    df %>%
      dplyr::group_by(!! key) %>%
      # TODO use better colname to avoid duplication
      dplyr::mutate(.result = !! predicate) %>%
      dplyr::ungroup() %>%
                   # dplyr::if_else() is a bit too strict here, where we don't know the type of key beforehand.
      dplyr::mutate(.group = !! key,
                    !! key := ifelse(.result, !! key, NA),
                    .alpha  = ifelse(.result, 1, 0.2))
  }
}

build_ungrouped_filter_func <- function(predicate) {
  function(df) {
    fdf <- dplyr::filter(df, !! predicate)
    dplyr::ungroup(fdf)
  }
}
