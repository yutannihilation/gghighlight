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
#' @export
geom_highlight <- function(...,
                           n = NULL,
                           max_highlight = 5L,
                           unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                           use_group_by = NULL) {
  structure(
    list(
      predicates = rlang::enquos(...),
      n = n,
      max_highlight = max_highlight,
      unhighlighted_colour = unhighlighted_colour,
      use_group_by = use_group_by
    ),
    class = "gg_highlighter"
  )
}

VERY_SECRET_GROUP_COLUMN_NAME <- rlang::sym("group..........")

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
  # Especially, group_key should be extracted here before it gets renamed to VERY_SECRET_GROUP_COLUMN_NAME.
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

  plot$layers[idx_layers] <- layers_bleached
  plot %+% layers_sieved
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
    layer$data <- dplyr::rename(layer$data, !!VERY_SECRET_GROUP_COLUMN_NAME := !!group_key)
    layer$mapping$group <- rlang::quo(!!VERY_SECRET_GROUP_COLUMN_NAME)
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
              "Falling back to ungrouped filter operation...")
      use_group_by <- FALSE
    }
  }

  names(predicates) <- paste0("p", seq_along(predicates))

  if (use_group_by) {
    data_predicated <- layer$data %>%
      # Rename group_key to prevent it from name collision.
      dplyr::rename(!!VERY_SECRET_GROUP_COLUMN_NAME := !! group_key) %>%
      dplyr::group_by(!!VERY_SECRET_GROUP_COLUMN_NAME) %>%
      dplyr::summarise(!!!predicates)

    cols_idx <- purrr::map_lgl(data_predicated, is.logical)
    cols_filter <- rlang::syms(names(cols_idx)[cols_idx])
    cols_arrange <- rlang::syms(names(cols_idx)[!cols_idx])

    data_filtered <- data_predicated %>%
      # first, fitler by the logical predicates
      dplyr::filter(!!!cols_filter) %>%
      # then, arrange by the other predicates
      # Note that desc() is different from dplyr::desc() (hint: hybrid evaluation)
      dplyr::arrange(desc(!!!cols_arrange)) %>%
      # slice down to max_highlight
      dplyr::slice(1:(!!max_highlight))

    groups_filtered <- dplyr::pull(data_filtered, !!VERY_SECRET_GROUP_COLUMN_NAME)

    layer$data <- dplyr::filter(layer$data, (!!group_key) %in% (!!groups_filtered))
  } else {
    data_predicated <- layer$data %>%
      # TODO: chage this to more improbable name
      tibble::rowid_to_column("rowid") %>%
      dplyr::transmute(!!! predicates, .data$rowid)

    cols_idx <- purrr::map_lgl(data_predicated, is.logical)
    cols_filter <- rlang::syms(names(cols_idx)[cols_idx])
    cols_arrange <- rlang::syms(names(cols_idx)[!cols_idx])

    data_filtered <- data_predicated %>%
      # first, fitler by the logical predicates
      dplyr::filter(!!!cols_filter) %>%
      # then, arrange by the other predicates
      dplyr::arrange(desc(!!!cols_arrange)) %>%
      # slice down to max_highlight
      dplyr::slice(1:(!!max_highlight))

    # sort to preserve the original order
    rowids_filtered <- sort(data_filtered$rowid)

    layer$data <- dplyr::slice(layer$data, !!rowids_filtered)
  }

  layer
}
