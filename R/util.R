# Utilities

build_grouped_filter_func <- function(predicate, key) {
  function(df) {
    df_grouped <- dplyr::group_by(df, !! key)
    df_filtered <- dplyr::filter(df_grouped, !! predicate)
    dplyr::ungroup(df_filtered)
  }
}

build_ungrouped_filter_func <- function(predicate) {
  function(df) {
    dplyr::filter(df, !! predicate)
  }
}
