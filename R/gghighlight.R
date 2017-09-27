#' Highlight Data With Predicate
#'
#' @inheritParams ggplot2::ggplot
#' @param predicate expression to filter data, which is passed to [dplyr::filter()].
#' @param unhighlighted_colour colour for unhighlited lines/points
#' @param geom name of geometry
#' @param ... arguments passed to the corresponding geometry functions (e.g. `geom_line()`)
#' @export
gghighlight <- function(data,
                        mapping,
                        predicate,
                        unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                        geom = c("line", "point"),
                        ...,
                        environment = parent.frame()) {

  geom <- match.arg(geom)
  predicate <- rlang::enquo(predicate)

  key <- infer_group_key_from_aes(mapping)

  data_filtered <- dplyr::group_by(data, !! key) %>%
    dplyr::filter(!! predicate) %>%
    dplyr::ungroup()

  mapping_unhighlitghted <- mapping
  mapping_unhighlitghted$colour <- NULL
  mapping_unhighlitghted$fill   <- NULL
  mapping_unhighlitghted$group  <- rlang::quo_expr(key)

  geom_func <- get(glue::glue("geom_{geom}"), envir = asNamespace("ggplot2"))

  ggplot2::ggplot(data = data_filtered, environment = environment) +
    geom_func(data = data,
              mapping = mapping_unhighlitghted,
              colour = unhighlighted_colour,
              ...) +
    geom_func(data = data_filtered,
              mapping = mapping,
              ...)
}

infer_group_key_from_aes <- function(mapping) {
  mapping$group %||% mapping$colour
}
