#' Highlight Data With Predicate
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("defunct")}
#'
#' `gghiglight_line()` and `gghighlight_point()` are deprecated. Please use [gghighlight()] instead.
#'
#' @name gghighlight-old
#'
#' @importFrom magrittr %>%
#' @inheritParams ggplot2::ggplot
#' @param predicate Expression to filter data, which is passed to [dplyr::filter()].
#' @param max_highlight Max number of series to highlight.
#' @param unhighlighted_colour Colour for unhighlighted lines/points.
#' @param use_group_by If `TRUE`, use [dplyr::group_by()] to evaluate `predicate`.
#' @param use_direct_label If `TRUE`, add labels directly on the plot instead of using a legend.
#' @param label_key Column name for `label` aesthetics.
#' @param ... Arguments passed to the corresponding geometry functions (e.g. `geom_line()`).
#'
#' @keywords internal
NULL

#' @rdname gghighlight-old
#' @export
gghighlight_line <- function(data,
                             mapping,
                             predicate,
                             max_highlight = 5L,
                             unhighlighted_colour = scales::alpha("grey", 0.7),
                             use_group_by = TRUE,
                             use_direct_label = TRUE,
                             label_key = NULL,
                             ...,
                             environment = parent.frame()) {

  lifecycle::deprecate_stop("0.1.0", "gghighlight_line()", with = "gghighlight()")
}


#' @rdname gghighlight-old
#' @export
gghighlight_point <- function(data,
                              mapping,
                              predicate,
                              max_highlight = 5L,
                              unhighlighted_colour = scales::alpha("grey", 0.7),
                              use_group_by = FALSE,
                              use_direct_label = TRUE,
                              label_key = NULL,
                              ...,
                              environment = parent.frame()) {

  lifecycle::deprecate_stop("0.1.0", "gghighlight_point()", with = "gghighlight()")
}
