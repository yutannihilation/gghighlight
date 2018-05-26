#' Clone Other Layers
#'
#' @param ...
#'   Expressions to filter data, which is passed to [dplyr::filter()].
#' @param n
#'   Number of layers to clone.
#' @param max_highlight
#'   Max number of series to highlight.
#' @param unhighlighted_colour
#'   Colour for unhighlited lines/points.
#' @param use_group_by
#'   If `TRUE`, use [dplyr::group_by()] to evaluate `predicate`.
#' @param use_direct_label
#'   If `TRUE`, add labels directly on the plot instead of using a legend.
#' @param label_key
#'   Column name for `label` aesthetics.
#' @inheritParams ggplot2::layer
#' @export
geom_highlight <- function(...,
                           n = NULL,
                           max_highlight = 5L,
                           unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                           use_group_by = NULL,
                           use_direct_label = NULL,
                           label_key = NULL) {

  # if use_direct_label is NULL, try to use direct labels but ignore failures
  # if use_direct_label is TRUE, use direct labels, otherwise stop()
  # if use_direct_label is FALSE, do not use direct labeys
  label_key_must_exist <- TRUE
  if (is.null(use_direct_label)) {
    use_direct_label <- TRUE
    label_key_must_exist <- FALSE
  }

  structure(
    list(
      predicates = rlang::enquos(...),
      n = n,
      max_highlight = max_highlight,
      unhighlighted_colour = unhighlighted_colour,
      use_group_by = use_group_by,
      use_direct_label = use_direct_label,
      label_key_must_exist = label_key_must_exist,
      label_key = rlang::enquo(label_key)
    ),
    class = "gg_highlighter"
  )
}

geom_highlight_with_preset <- function(geom_func, ...,
                                       n,
                                       max_highlight,
                                       unhighlighted_colour,
                                       use_group_by,
                                       use_direct_label,
                                       label_key) {
  dots <- rlang::enquos(...)
  idx <- rlang::have_name(dots)
  # named arguments are for geom_func
  geom_args <- dots[idx]
  # unnamed arguments are for predicats
  predicates <- dots[!idx]

  geom_quo <- rlang::quo((!!geom_func)(!!!geom_args))
  label_key <- rlang::enquo(label_key)

  list(
    rlang::eval_tidy(geom_quo),
    geom_highlight(
      !!!predicates,
      n = n, max_highlight = max_highlight, unhighlighted_colour = unhighlighted_colour,
      use_group_by = use_group_by, use_direct_label = use_direct_label, label_key = !!label_key
    )
  )
}

#' @rdname geom_highlight
#' @export
geom_point_highlight <- function(...,
                                 n = NULL,
                                 max_highlight = 5L,
                                 unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                                 use_group_by = NULL,
                                 use_direct_label = NULL,
                                 label_key = NULL) {
  label_key <- rlang::enquo(label_key)
  geom_highlight_with_preset(
    geom_func = ggplot2::geom_point,
    ..., n = n, max_highlight = max_highlight, unhighlighted_colour = unhighlighted_colour,
    use_group_by = use_group_by, use_direct_label = use_direct_label, label_key = !!label_key
  )
}

#' @rdname geom_highlight
#' @export
geom_line_highlight <- function(...,
                                n = NULL,
                                max_highlight = 5L,
                                unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                                use_group_by = NULL,
                                use_direct_label = NULL,
                                label_key = NULL) {
  label_key <- rlang::enquo(label_key)
  geom_highlight_with_preset(
    geom_func = ggplot2::geom_line,
    ..., n = n, max_highlight = max_highlight, unhighlighted_colour = unhighlighted_colour,
    use_group_by = use_group_by, use_direct_label = use_direct_label, label_key = !!label_key
  )
}


VERY_SECRET_COLUMN_NAME <- rlang::sym("highlight..........")

#' @export
ggplot_add.gg_highlighter <- function(object, plot, object_name) {
  if (length(plot$layers) == 0) {
    stop("there is no layer to highlight!")
  }

  if (!is.null(object$n)) {
    n_layers <- length(plot$layers)
    if (object$n > object$n) {
      stop("n is larger than the actual number of layers!")
    }
    idx_layers <- utils::tail(seq_len(n_layers), object$n)
  } else {
    idx_layers <- seq_along(plot$layers)
  }

  # Layers are environments; if we modify an element of it, it keeps the modified value.
  # So, we need to clone them first.
  layers_cloned <- purrr::map(plot$layers[idx_layers], clone_layer)

  # data and group_keys are used commonly both in the bleaching and sieving process.
  # Especially, group_key should be extracted here before it gets renamed to VERY_SECRET_COLUMN_NAME.
  purrr::walk(layers_cloned, merge_plot_to_layer,
              plot_data = plot$data, plot_mapping = plot$mapping)
  group_keys <- purrr::map(layers_cloned, ~ infer_group_key_from_aes(.$mapping))

  # Clone layers again before we bleach them.
  layers_bleached <- layers_cloned
  layers_sieved <- purrr::map(layers_bleached, clone_layer)

  # Bleach the lower layer.
  purrr::walk2(
    layers_bleached,
    group_keys,
    bleach_layer,
    unhighlighted_colour = object$unhighlighted_colour
  )

  # Sieve the upper layer.
  purrr::walk2(
    layers_sieved,
    group_keys,
    sieve_layer,
    predicates = object$predicates,
    max_highlight = object$max_highlight,
    use_group_by = object$use_group_by
  )

  # The plot data should also be sieved to deleting facets for unhighlighted levels
  # TODO: This may be treated more carefully.
  # c.f. https://github.com/yutannihilation/gghighlight/pull/26#issuecomment-391332229
  plot$data <- layers_sieved[[1]]$data

  plot$layers[idx_layers] <- layers_bleached
  plot <- plot %+% layers_sieved

  if (!object$use_direct_label) {
    return(plot)
  }

  layer_labelled <- generate_labelled_layer(layers_sieved, group_keys, object$label_key)

  if (is.null(layer_labelled)) {
    if (object$label_key_must_exist) {
      stop("No layer can be used for labels", call. = FALSE)
    } else {
      return(plot)
    }
  }

  plot %+% layer_labelled %+% ggplot2::guides(colour = "none", fill = "none")
}

merge_plot_to_layer <- function(layer, plot_data, plot_mapping) {
  layer$data <- merge_data(layer, plot_data)
  layer$mapping <- merge_mapping(layer, plot_mapping)
  layer
}

merge_data <- function(layer, plot_data) {
  # c.f.) https://github.com/tidyverse/ggplot2/blob/54de616213d9811f422f45cf1a6c04d1de6ccaee/R/layer.r#L182
  layer$layer_data(plot_data)
}

merge_mapping <- function(layer, plot_mapping) {
  # Merge the layer's mapping with the plot's mapping
  mapping <- utils::modifyList(plot_mapping %||% aes(), layer$mapping %||% aes())
  # Filter out unused variables (e.g. fill aes for line geom)
  aes_names <- base::intersect(layer$geom$aesthetics(), names(mapping))
  mapping <- mapping[aes_names]

  if (length(mapping) == 0) {
    stop("No mapping found on this layer!")
  }

  mapping
}

clone_layer <- function(layer) {
  new_layer <- rlang::env_clone(layer)
  class(new_layer) <- class(layer)
  new_layer
}

bleach_layer <- function(layer, group_key,
                         unhighlighted_colour  = ggplot2::alpha("grey", 0.7)) {
  # set colour and fill to grey only when it is included in the mappping
  # (Note that this needs to be executed before modifying the layer$mapping)
  params_bleached <- list()
  aes_names <- base::intersect(c("colour", "fill"), layer$geom$aesthetics())
  params_bleached[aes_names] <- unhighlighted_colour
  layer$aes_params <- utils::modifyList(layer$aes_params, params_bleached)

  # remove colour and fill from mapping
  layer$mapping[c("colour", "fill")] <- list(NULL)

  if (!is.null(group_key)) {
    # Add group var to preserver implicit grouping to prevent the bleached
    # data to facetted, rename the group column to the very improbable name.
    # e.g. geom_line(aes(colour = c)); if colour aes is removed, line will be drawn unintentionally.
    # But, is group key always needed...? (e.g. points)
    layer$data <- dplyr::rename(layer$data, !!VERY_SECRET_COLUMN_NAME := !!group_key)
    layer$mapping$group <- rlang::quo(!!VERY_SECRET_COLUMN_NAME)
  }

  layer
}

sieve_layer <- function(layer, group_key, predicates,
                        max_highlight = 5L,
                        use_group_by = NULL) {
  # If there are no predicates, do nothing.
  if (length(predicates) == 0) return(layer)

  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_key)

  # 1) If use_group_by is FALSE, do not use group_by().
  # 2) If use_group_by is TRUE and group_key is not NULL, use group_by().
  # 3) If use_group_by is TRUE but group_key is NULL, show a warning and do not use group_by().
  if (use_group_by) {
    if (is.null(group_key)) {
      warning("You set use_group_by = TRUE, but there seems no group_key.\n",
              "Please provide group, colour or fill aes.\n",
              "Falling back to ungrouped filter operation...", call. = FALSE)
      use_group_by <- FALSE
    }
  }

  names(predicates) <- paste0("p", seq_along(predicates))

  # If use_group_by is TRUE, try to calculate grouped
  if (use_group_by) {
    tryCatch({
      layer$data <- calculate_grouped(layer$data, predicates, max_highlight, group_key)
      # if this succeeds, return the layer
      return(layer)
    },
    error = function(e) {
      warning("You set use_group_by = TRUE, but grouped calculation failed.\n",
              "Falling back to ungrouped filter operation...", call. = FALSE)
    })
  }

  # the grouped calculation failed or skipped, try ungrouped one.
  layer$data <- calculate_ungrouped(layer$data, predicates, max_highlight)
  layer
}

calculate_grouped <- function(data, predicates, max_highlight, group_key) {
  data_predicated <- data %>%
    dplyr::group_by(!!group_key) %>%
    dplyr::summarise(!!!predicates)

  col_idx <- data_predicated %>%
    # Do not use group_key to arrange.
    dplyr::select(-!!group_key) %>%
    purrr::map_lgl(is.logical)
  cols_filter <- rlang::syms(names(col_idx)[col_idx])
  cols_arrange <- rlang::syms(names(col_idx)[!col_idx])

  # Filter by the logical predicates.
  data_filtered <- data_predicated %>%
    dplyr::filter(!!!cols_filter)

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols_arrange) > 0) {
    data_filtered <- data_filtered %>%
      dplyr::arrange(!!!cols_arrange) %>%
      utils::tail(max_highlight)
  }

  groups_filtered <- dplyr::pull(data_filtered, !!group_key)

  dplyr::filter(data, (!!group_key) %in% (!!groups_filtered))
}

calculate_ungrouped <- function(data, predicates, max_highlight) {
  data_predicated <- data %>%
    tibble::rowid_to_column(var = rlang::expr_text(VERY_SECRET_COLUMN_NAME)) %>%
    dplyr::transmute(!!! predicates, !!VERY_SECRET_COLUMN_NAME)

  col_idx <- data_predicated %>%
    # Do not use row IDs to arrange.
    dplyr::select(-!!VERY_SECRET_COLUMN_NAME) %>%
    purrr::map_lgl(is.logical)
  cols_filter <- rlang::syms(names(col_idx)[col_idx])
  cols_arrange <- rlang::syms(names(col_idx)[!col_idx])

  # Filter by the logical predicates.
  data_filtered <- data_predicated %>%
    dplyr::filter(!!!cols_filter)

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols_arrange) > 0) {
    data_filtered <- data_filtered %>%
      dplyr::arrange(!!!cols_arrange) %>%
      utils::tail(max_highlight)
  }

  # sort to preserve the original order
  rowids_filtered <- sort(dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME))

  data[rowids_filtered, ]
}
