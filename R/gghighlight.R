#' Highlight Data With Predicate
#'
#' `gghiglight()` highlights (almost) any geoms according to the given predicates.
#'
#' @name gghighlight
#'
#' @param ...
#'   Expressions to filter data, which is passed to [dplyr::filter()].
#' @param n
#'   Number of layers to clone.
#' @param max_highlight
#'   Max number of series to highlight.
#' @param unhighlighted_aes
#'   Aesthetics (e.g. colour, fill, and size) for unhighlighted geoms.
#' @param use_group_by
#'   If `TRUE`, use [dplyr::group_by()] to evaluate `predicate`.
#' @param use_direct_label
#'   If `TRUE`, add labels directly on the plot instead of using a legend.
#' @param label_key
#'   Column name for `label` aesthetics.
#' @param label_params
#'   A list of parameters, which is passed to [ggrepel::geom_label_repel()].
#' @param unhighlighted_colour
#'   (Deprecated) Colour for unhighlighted geoms.
#'
#' @examples
#' d <- data.frame(
#'   idx = c( 1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   value = c( 1, 2, 3,10,11,12, 9,10,11),
#'   category = rep(c("a","b","c"), 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # highlight the lines whose max values are larger than 10
#' ggplot(d, aes(idx, value, colour = category)) +
#'   geom_line() + gghighlight(max(value) > 10)
#'
#' # highlight the points whose values are larger than 10
#' ggplot(d, aes(idx, value)) +
#'   geom_point() +
#'   gghighlight(value > 10, label_key = category)
#'
#' @export
gghighlight <- function(...,
                        n = NULL,
                        max_highlight = 5L,
                        unhighlighted_aes = list(colour = ggplot2::alpha("grey", 0.7)),
                        use_group_by = NULL,
                        use_direct_label = NULL,
                        label_key = NULL,
                        label_params = list(fill = "white"),
                        unhighlighted_colour = NULL) {

  # if use_direct_label is NULL, try to use direct labels but ignore failures
  # if use_direct_label is TRUE, use direct labels, otherwise stop()
  # if use_direct_label is FALSE, do not use direct labeys
  label_key_must_exist <- TRUE
  if (is.null(use_direct_label)) {
    use_direct_label <- TRUE
    label_key_must_exist <- FALSE
  }

  # if fill is not specified, use colour for fill, or vice versa
  unhighlighted_aes <- normalize_unhighlighted_aes(unhighlighted_aes)

  if (!is.null(unhighlighted_colour)) {
    rlang::warn("unhighlighted_colour is deprecated. Use unhighlighted_aes instead.")
    unhighlighted_aes$colour <- unhighlighted_colour
  }

  structure(
    list(
      predicates = rlang::enquos(...),
      n = n,
      max_highlight = max_highlight,
      unhighlighted_aes = unhighlighted_aes,
      use_group_by = use_group_by,
      use_direct_label = use_direct_label,
      label_key_must_exist = label_key_must_exist,
      label_key = rlang::enquo(label_key),
      label_params = label_params
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

  n_layers <- length(plot$layers)
  if (is.null(object$n)) {
    idx_layers <- rep(TRUE, n_layers)
  } else {
    if (object$n > n_layers) {
      stop("n is larger than the actual number of layers!", call. = FALSE)
    }
    idx_layers <- rep(FALSE, n_layers)
    idx_layers[utils::tail(seq_len(n_layers), object$n)] <- TRUE
  }

  # Layers are environments; if we modify an element of it, it keeps the modified value.
  # So, we need to clone them first.
  layers_cloned <- purrr::map(plot$layers, clone_layer)

  # data and group IDs are used commonly both in the bleaching and sieving process.
  purrr::walk(layers_cloned, merge_plot_to_layer,
              plot_data = plot$data, plot_mapping = plot$mapping)

  # since the plot data is overwritten later, we need to attach the data to all layers (#31)
  plot$layers[!idx_layers] <- layers_cloned[!idx_layers]
  layers_cloned <- layers_cloned[idx_layers]

  group_infos <- purrr::map(layers_cloned, ~ calculate_group_info(.$data, .$mapping))

  # Clone layers again before we bleach them.
  layers_bleached <- layers_cloned
  layers_sieved <- purrr::map(layers_bleached, clone_layer)

  # Bleach the lower layer.
  purrr::walk2(
    layers_bleached,
    group_infos,
    bleach_layer,
    unhighlighted_aes = object$unhighlighted_aes
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
  plot$data <- layers_sieved[[1]]$data

  plot$layers[idx_layers] <- layers_bleached
  plot <- plot %+% layers_sieved

  if (!object$use_direct_label) {
    return(plot)
  }

  layer_labelled <- generate_labelled_layer(layers_sieved, group_infos,
                                            object$label_key, object$label_params)

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
  # since gghighlight does grouped calculations, we want to specify the group key by ourselves.
  layer$data <- dplyr::ungroup(layer$data)
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

clone_position <- clone_layer

calculate_group_info <- function(data, mapping) {
  mapping <- purrr::compact(mapping)
  # the calculation may be possible only in the ggplot2 context (e.g. stat()).
  # So, wrap it with tryCatch() and remove the failed results, which can be
  # detected by checking if all elements are NA or not.
  mapping_wrapped <- purrr::map(mapping, ~ rlang::quo(tryCatch(!!., error = function(e) NA)))
  data_evaluated <- dplyr::transmute(data, !!!mapping_wrapped)
  data_evaluated <- dplyr::select_if(data_evaluated, ~!all(is.na(.)))

  idx_discrete <- purrr::map_lgl(data_evaluated, ~ is.factor(.) || is.character(.) || is.logical(.))
  if (!any(idx_discrete)) {
    return(NULL)
  } else {
    aes_discrete <- names(which(idx_discrete))
    list(
      data = data_evaluated,
      # Calculate group IDs as ggplot2 does. (c.f. https://github.com/tidyverse/ggplot2/blob/8778b48b37d8b7e41c0f4f213031fb47810e70aa/R/grouping.r#L11-L28)
      id = dplyr::group_indices(
        data_evaluated,  # group_indices() won't work with grouped_df.
        !!!rlang::syms(aes_discrete)
      ),
      # for group key, use symbols only
      key = purrr::keep(mapping[aes_discrete], rlang::quo_is_symbol)
    )
  }
}

bleach_layer <- function(layer, group_info, unhighlighted_aes) {

  # c.f. https://github.com/tidyverse/ggplot2/blob/e9d4e5dd599b9f058cbe9230a6517f85f3587567/R/layer.r#L107-L108
  aes_params_bleached <- unhighlighted_aes[names(unhighlighted_aes) %in% layer$geom$aesthetics()]
  geom_params_bleached <- unhighlighted_aes[names(unhighlighted_aes) %in% layer$geom$parameters(TRUE)]

  # Use the colour and fill specified in unhighlighted_aes when it is included in
  # the mappping. But, if the default_aes is NA, respect it.
  # (Note that this needs to be executed before modifying the layer$mapping)
  aes_params_bleached <- fill_unhighlighted_aes_with_na(aes_params_bleached, layer$geom, layer$mapping)

  layer$aes_params <- utils::modifyList(layer$aes_params, aes_params_bleached)
  layer$geom_params <- utils::modifyList(layer$geom_params, geom_params_bleached)

  # remove colour and fill from mapping
  layer$mapping[c("colour", "fill")] <- list(NULL)

  if (!is.null(group_info$key)) {
    # In order to prevent the bleached layer to be facetted, we need to rename
    # columns of group keys to improbable names. But, what happens when the group
    # column disappears? Other calculations that uses the column fail. So, we need
    # to use the pre-evaluated values and rename everything to improbable name.
    layer$data <- group_info$data
    secret_prefix <- rlang::expr_text(VERY_SECRET_COLUMN_NAME)
    mapping_names <- names(layer$data)
    secret_names <- paste0(secret_prefix, seq_along(mapping_names))
    secret_quos <- rlang::quos(!!!rlang::syms(secret_names))
    layer$data <- dplyr::rename(layer$data, !!!stats::setNames(mapping_names, secret_names))
    layer$mapping <- utils::modifyList(layer$mapping, stats::setNames(secret_quos, mapping_names))

    secret_name_group <- paste0(secret_prefix, "group")
    layer$data[secret_name_group] <- factor(group_info$id)
    layer$mapping$group <- rlang::quo(!!rlang::sym(secret_name_group))
  }

  layer
}

fill_unhighlighted_aes_with_na <- function(unhighlighted_aes, geom, mapping) {
  aes_name <- names(unhighlighted_aes)

  # if aes_name is not specified in the mapping and the default_aes is NA, use NA.
  is_default_na <- !aes_name %in% names(mapping) &
    aes_name %in% names(geom$default_aes) &
    is.na(geom$default_aes[aes_name])

  unhighlighted_aes[is_default_na] <- NA

  unhighlighted_aes
}

sieve_layer <- function(layer, group_info, predicates,
                        max_highlight = 5L,
                        use_group_by = NULL) {
  # If there are no predicates, do nothing.
  if (length(predicates) == 0) return(layer)

  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)

  # 1) If use_group_by is FALSE, do not use group_by().
  # 2) If use_group_by is TRUE and group IDs don't exist, use group_by().
  # 3) If use_group_by is TRUE but group IDs exist, show a warning and do not use group_by().
  if (use_group_by) {
    if (is.null(group_info$id)) {
      warning("You set use_group_by = TRUE, but there seems no groups.\n",
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

  data[group_ids %in% groups_filtered, ]
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

normalize_unhighlighted_aes <- function(aes_params) {
  if (!is.list(aes_params)) {
    rlang::abort("unhighlighted_aes must be a list.")
  }

  # color is an alias of colour
  if (!is.null(aes_params$color)) {
    aes_params$colour <- aes_params$colour %||% aes_params$color
    aes_params$color <- NULL
  }

  # if fill or colour is missing, use the other for it
  if (is.null(aes_params$colour) && is.null(aes_params$fill)) {
    rlang::abort("unhighlighted_aes must contain at least either of colour or fill.")
  }

  aes_params$colour <- aes_params$colour %||% aes_params$fill
  aes_params$fill <- aes_params$fill %||% aes_params$colour
  aes_params
}
