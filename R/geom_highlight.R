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
  group_infos <- purrr::map(layers_cloned, ~ calculate_group_info(.$data, .$mapping))

  # Clone layers again before we bleach them.
  layers_bleached <- layers_cloned
  layers_sieved <- purrr::map(layers_bleached, clone_layer)

  # Bleach the lower layer.
  purrr::walk2(
    layers_bleached,
    group_infos,
    bleach_layer,
    unhighlighted_colour = object$unhighlighted_colour
  )

  # Sieve the upper layer.
  purrr::walk2(
    layers_sieved,
    group_infos,
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

  layer_labelled <- generate_labelled_layer(layers_sieved, purrr::map(group_infos, "id"), object$label_key)

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
  layer_aes <- base::union(layer$geom$aesthetics(), layer$stat$aesthetics())
  aes_names <- base::intersect(layer_aes, names(mapping))
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

calculate_group_info <- function(data, mapping) {
  mapping <- purrr::compact(mapping)
  data_evaluated <- dplyr::transmute(data, !!!mapping)

  idx_discrete <- purrr::map_lgl(data_evaluated, ~ is.factor(.) || is.character(.) || is.logical(.))
  if (!any(idx_discrete)) {
    return(NULL)
  } else {
    aes_discrete <- names(which(idx_discrete))
    list(
      # Calculate group IDs as ggplot2 does. (c.f. https://github.com/tidyverse/ggplot2/blob/8778b48b37d8b7e41c0f4f213031fb47810e70aa/R/grouping.r#L11-L28)
      id = dplyr::group_indices(data_evaluated, !!!rlang::syms(aes_discrete)),
      # Try to rename the column into improbable name so that the bleached layer won't get facetted.
      key = purrr::keep(mapping[aes_discrete], rlang::quo_is_symbol)
    )
  }
}

bleach_layer <- function(layer, group_info,
                         unhighlighted_colour  = ggplot2::alpha("grey", 0.7)) {
  # set colour and fill to grey only when it is included in the mappping
  # (Note that this needs to be executed before modifying the layer$mapping)
  params_bleached <- list()
  aes_names <- base::intersect(c("colour", "fill"), layer$geom$aesthetics())
  params_bleached[aes_names] <- unhighlighted_colour
  layer$aes_params <- utils::modifyList(layer$aes_params, params_bleached)

  # remove colour and fill from mapping
  layer$mapping[c("colour", "fill")] <- list(NULL)

  if (!is.null(group_info$key)) {
    # Add group var to preserver implicit grouping to prevent the bleached
    # data to facetted, rename the group column to the very improbable name.
    # e.g. geom_line(aes(colour = c)); if colour aes is removed, line will be drawn unintentionally.
    # But, is group key always needed...? (e.g. points)
    secret_colname <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
    names(group_info$key) <- paste0(secret_colname, seq_along(group_info$key))
    layer$data <- dplyr::rename(layer$data, !!!group_info$key)
    layer$data[secret_colname] <- factor(group_info$id)
    layer$mapping$group <- rlang::quo(!!VERY_SECRET_COLUMN_NAME)
  }

  layer
}

sieve_layer <- function(layer, group_info, predicates,
                        max_highlight = 5L,
                        use_group_by = NULL) {
  # If there are no predicates, do nothing.
  if (length(predicates) == 0) return(layer)

  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)

  # 1) If use_group_by is FALSE, do not use group_by().
  # 2) If use_group_by is TRUE and group_key is not NULL, use group_by().
  # 3) If use_group_by is TRUE but group_key is NULL, show a warning and do not use group_by().
  if (use_group_by) {
    if (is.null(group_info$id)) {
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
      layer$data <- calculate_grouped(layer$data, predicates, max_highlight, group_info$id)
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

calculate_grouped <- function(data, predicates, max_highlight, group_ids) {
  data_predicated <- data %>%
    dplyr::group_by(!!VERY_SECRET_COLUMN_NAME := !!group_ids) %>%
    dplyr::summarise(!!!predicates)

  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)

  # Filter by the logical predicates.
  data_filtered <- data_predicated %>%
    dplyr::filter(!!!cols$filter)

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- data_filtered %>%
      dplyr::arrange(!!!cols$arrange) %>%
      utils::tail(max_highlight)
  }

  groups_filtered <- dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME)

  dplyr::filter(data, (!!VERY_SECRET_COLUMN_NAME) %in% (!!groups_filtered))
}

calculate_ungrouped <- function(data, predicates, max_highlight) {
  data_predicated <- data %>%
    tibble::rowid_to_column(var = rlang::expr_text(VERY_SECRET_COLUMN_NAME)) %>%
    dplyr::transmute(!!! predicates, !!VERY_SECRET_COLUMN_NAME)

  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)

  # Filter by the logical predicates.
  data_filtered <- data_predicated %>%
    dplyr::filter(!!!cols$filter)

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- data_filtered %>%
      dplyr::arrange(!!!cols$arrange) %>%
      utils::tail(max_highlight)
  }

  # sort to preserve the original order
  rowids_filtered <- sort(dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME))

  data[rowids_filtered, ]
}

choose_col_for_filter_and_arrange <- function(data, exclude_col) {
  # Do not use row IDs or group keys to arrange.
  data <- dplyr::select(data, -!!exclude_col)
  col_idx_lgl <- purrr::map_lgl(data, is.logical)
  col_idx_lst <- purrr::map_lgl(data, is.list)
  list(
    # Use logical columns for filter()
    filter = rlang::syms(names(data)[col_idx_lgl]),
    # Use other columns but lists for arrange() (arrange doesn't support list columns)
    arrange = rlang::syms(names(data)[!col_idx_lgl & !col_idx_lst])
  )
}
