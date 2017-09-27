#' Highlight Data With Predicate
#'
#' @inheritParams ggplot2::ggplot
#' @param predicate Expression to filter data, which is passed to [dplyr::filter()].
#' @param unhighlighted_colour Colour for unhighlited lines/points.
#' @param use_group_by If \code{TRUE}, apply \code{filter} on the grouped data.
#' @param ... Arguments passed to the corresponding geometry functions (e.g. `geom_line()`).
#'
#' @examples
#' d <- data.frame(
#'   idx = c( 1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   value = c( 1, 2, 3,10,11,12, 9,10,11),
#'   category = rep(c("a","b","c"), 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' gghighlight_line(d, aes(idx, value, colour = category),
#'                  max(value) > 10)
#'
gghighlight <- function(data,
                        mapping,
                        predicate_quo,
                        unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                        geom_func = ggplot2::geom_blank,
                        use_group_by = TRUE,
                        ...,
                        environment = parent.frame()) {


  group_key <- if (use_group_by) infer_group_key_from_aes(mapping) else NULL

  if (use_group_by && is.null(group_key)) {
    warning("Please provide group or colour aes.\n",
            "Falling back to ungrouped filter operation...")
  }

  mapping_unhighlitghted <- mapping
  # https://cran.r-project.org/doc/FAQ/R-FAQ.html#Others
  mapping_unhighlitghted["colour"] <- list(NULL)
  mapping_unhighlitghted["fill"]   <- list(NULL)

  if (!is.null(group_key)) {
    data_filtered <- dplyr::group_by(data, !! group_key) %>%
      dplyr::filter(!! predicate_quo)

    mapping_unhighlitghted$group  <- group_key
  } else {
    data_filtered <- dplyr::filter(data, !! predicate_quo)
  }

  # base plot
  ggplot2::ggplot(data = data_filtered,
                  mapping = mapping,
                  environment = environment) +
    # unhighlighted plot
    geom_func(data = data,
              mapping = mapping_unhighlitghted,
              colour = unhighlighted_colour,
              ...) +
    # highlighted plot
    geom_func(data = data_filtered,
              mapping = mapping,
              ...)
}

infer_group_key_from_aes <- function(mapping) {
  mapping$group %||% mapping$colour
}


#' @rdname gghighlight
#' @export
gghighlight_line <- function(data,
                             mapping,
                             predicate,
                             unhighlighted_colour = ggplot2::alpha("grey", 0.3),
                             use_group_by = TRUE,
                             use_direct_label = TRUE,
                             ...,
                             environment = parent.frame()) {

  p <- gghighlight(data = data,
                   mapping = mapping,
                   predicate_quo = rlang::enquo(predicate),
                   unhighlighted_colour = unhighlighted_colour,
                   geom_func = ggplot2::geom_line,
                   use_group_by = use_group_by,
                   ...,
                   environment = environment)

  if (!use_direct_label) return(p)

  layer_highlight <- p$layers[[2]]
  data_highlight <- layer_highlight$data
  # data_highlight is a grouped df
  group_key <- dplyr::groups(data_highlight)[[1]]

  if (is.null(group_key)) {
    warning("No grouped vars.\n",
            "Falling back to a usual legend...")
    return(p)
  }

  mapping_label <- layer_highlight$mapping
  mapping_label$label <- group_key

  x_key <- mapping_label$x

  leftmost_points <- data_highlight %>%
    dplyr::filter((!! x_key) == max(!! x_key))

  p +
    ggplot2::guides(colour=FALSE) +
    ggrepel::geom_label_repel(data = leftmost_points,
                              mapping = mapping_label)
}
