#' Highlight Data With Predicate
#'
#' `gghiglight_line()` and `gghighlight_point()` are deprecated. Please use [gghighlight()] instead.
#'
#' @name gghighlight-old
#'
#' @inheritParams ggplot2::ggplot
#' @param predicate Expression to filter data, which is passed to [dplyr::filter()].
#' @param max_highlight Max number of series to highlight.
#' @param unhighlighted_colour Colour for unhighlighted lines/points.
#' @param use_group_by If `TRUE`, use [dplyr::group_by()] to evaluate `predicate`.
#' @param use_direct_label If `TRUE`, add labels directly on the plot instead of using a legend.
#' @param label_key Column name for `label` aesthetics.
#' @param ... Arguments passed to the corresponding geometry functions (e.g. `geom_line()`).
#'
NULL


gghighlight_old <- function(data,
                            mapping,
                            predicate_quo,
                            max_highlight = 5L,
                            unhighlighted_colour = ggplot2::alpha("grey", 0.7),
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
  aes_null <- intersect(c("colour", "fill"), names(mapping_unhighlitghted))
  mapping_unhighlitghted[aes_null] <- list(NULL)

  if (!is.null(group_key)) {
    data_grouped <- dplyr::group_by(data, !! group_key)
    # assume that no one use this silly colname :P
    data_predicated <- dplyr::summarise(data_grouped, predicate.......... = !! predicate_quo)

    groups <- if (is.logical(data_predicated$predicate..........)) {
      data_predicated[[quo_text(group_key)]][data_predicated$predicate..........]
    } else {
      data_predicated[[quo_text(group_key)]][order(data_predicated$predicate.........., decreasing = TRUE)][1:max_highlight]
    }

    data_filtered <- dplyr::filter(data_grouped, (!! group_key) %in% (!! groups))

    # rename the column for group key in original column so that this cannot be faccetted.
    # assume that no one use this silly colname :P
    data <- dplyr::rename(data, group.......... = !! group_key)
    mapping_unhighlitghted$group  <- sym("group..........")
  } else {
    data_predicated <- dplyr::mutate(data, predicate.......... = !! predicate_quo)
    if (is.logical(data_predicated$predicate..........)) {
      data_filtered <- dplyr::filter(data_predicated, .data$predicate..........)
    } else {
      data_predicated <- dplyr::arrange(data_predicated, -.data$predicate..........)
      data_filtered <- dplyr::slice(data_predicated, !! 1:max_highlight)
    }
  }

  # base plot
  ggplot2::ggplot(data = data_filtered,
                  mapping = mapping,
                  environment = environment) %+%
    # unhighlighted plot
    geom_func(data = data,
              mapping = mapping_unhighlitghted,
              colour = unhighlighted_colour,
              ...) %+%
    # highlighted plot
    geom_func(data = data_filtered,
              mapping = mapping,
              ...)
}

infer_group_key_from_aes <- function(mapping) {
  mapping$group %||% mapping$fill %||% mapping$colour
}


#' @rdname gghighlight-old
#' @export
gghighlight_line <- function(data,
                             mapping,
                             predicate,
                             max_highlight = 5L,
                             unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                             use_group_by = TRUE,
                             use_direct_label = TRUE,
                             label_key = NULL,
                             ...,
                             environment = parent.frame()) {

  p <- gghighlight_old(data = data,
                       mapping = mapping,
                       predicate_quo = enquo(predicate),
                       max_highlight = max_highlight,
                       unhighlighted_colour = unhighlighted_colour,
                       geom_func = ggplot2::geom_line,
                       use_group_by = use_group_by,
                       ...,
                       environment = environment)

  if (!use_direct_label) return(p)

  layer_highlight <- p$layers[[2]]
  data_highlight <- layer_highlight$data
  # data_highlight is a grouped df
  label_key <- dplyr::groups(data_highlight)[[1]] %||% substitute(label_key)

  if (is.null(label_key)) {
    warning("No grouped vars or label_key.\n",
            "Falling back to a usual legend...")
    return(p)
  }

  mapping_label <- layer_highlight$mapping
  mapping_label$label <- label_key

  x_key <- mapping_label$x

  leftmost_points <- data_highlight %>%
    dplyr::filter((!! x_key) == max(!! x_key))

  p %+%
    ggplot2::guides(colour=FALSE) %+%
    ggrepel::geom_label_repel(data = leftmost_points,
                              mapping = mapping_label)
}


#' @rdname gghighlight-old
#' @export
gghighlight_point <- function(data,
                              mapping,
                              predicate,
                              max_highlight = 5L,
                              unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                              use_group_by = FALSE,
                              use_direct_label = TRUE,
                              label_key = NULL,
                              ...,
                              environment = parent.frame()) {

  p <- gghighlight_old(data = data,
                       mapping = mapping,
                       predicate_quo = enquo(predicate),
                       max_highlight = max_highlight,
                       unhighlighted_colour = unhighlighted_colour,
                       geom_func = ggplot2::geom_point,
                       use_group_by = use_group_by,
                       ...,
                       environment = environment)

  if (!use_direct_label) return(p)

  layer_highlight <- p$layers[[2]]
  data_highlight <- layer_highlight$data
  mapping_highlight <- layer_highlight$mapping

  mapping_highlight$label <- substitute(label_key)
  if (is.null(mapping_highlight$label)) {
    col_labelable_idx <- which(
      purrr::map_lgl(data_highlight, is.character) | purrr::map_lgl(data_highlight, is.factor)
    )

    if (length(col_labelable_idx) == 0) {
      warning("Please provide the proper label_key.\n",
              "Falling back to a usual legend...")
      return(p)
    }

    col_labelable <- colnames(data_highlight)[col_labelable_idx[1]]
    mapping_highlight$label <- sym(col_labelable)
    warning(
      sprintf("Using %s as label for now, but please provide the label_key explicity!",
              col_labelable)
    )
  }

  p %+%
    ggplot2::guides(colour=FALSE) %+%
    ggrepel::geom_label_repel(data = data_highlight,
                              mapping = mapping_highlight)
}
