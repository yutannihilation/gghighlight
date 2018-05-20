#' Clone Other Layers
#'
#' @param ...
#'   Expressions to filter data, which is passed to [dplyr::filter()].
#' @param n
#'   Number of layers to clone.
#' @param unhighlighted_colour
#'   Colour for unhighlited lines/points.
#' @param max_highlight
#'   Max number of series to highlight.
#' @export
geom_highlight <- function(...,
                           n = NULL,
                           unhighlighted_colour = ggplot2::alpha("grey", 0.7),
                           max_highlight = 5L) {
  structure(
    list(
      predicates = rlang::enquos(...),
      n = n,
      unhighlighted_colour = unhighlighted_colour,
      max_highlight = max_highlight
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
  data_list <- purrr::map(layers_cloned, merge_data, plot_data = plot$data)
  mapping_list <- purrr::map(layers_cloned, merge_mapping, plot_mapping = plot$mapping)
  group_keys <- purrr::map(mapping_list, "group")

  # Clone layers again before we bleach them.
  layers_bleached <- layers_cloned
  layers_sieved <- purrr::map(plot$layers[idx_layers], clone_layer)

  # Bleach the lower layer.
  purrr::pwalk(
    list(
      layer = layers_bleached,
      data = data_list,
      mapping = mapping_list,
      group_key = group_keys
    ),
    bleach_layer,
    unhighlighted_colour = object$unhighlighted_colour
  )

  # Sieve the upper layer.
  purrr::pwalk(
    list(
      layer = layers_sieved,
      data = data_list,
      mapping = mapping_list,
      group_key = group_keys
    ),
    sieve_layer,
    predicates = object$predicates
  )

  plot$layers[idx_layers] <- layers_bleached
  plot %+% layers_sieved
}

merge_data <- function(layer, plot_data) {
  # c.f.) https://github.com/tidyverse/ggplot2/blob/54de616213d9811f422f45cf1a6c04d1de6ccaee/R/layer.r#L182
  layer$layer_data(plot_data)
}

merge_mapping <- function(layer, plot_mapping) {
  mapping <- merge_aes(layer$mapping, plot_mapping, layer$geom$aesthetics())
  if (length(mapping) == 0) {
    stop("No mapping found on this layer!")
  }
  group_key <- infer_group_key_from_aes(mapping)
  mapping$group <- group_key
  mapping
}

bleach_layer <- function(layer, data, mapping, group_key, unhighlighted_colour) {
  colour_aes <- mapping$colour
  fill_aes <- mapping$fill

  if (is.null(colour_aes) && is.null(fill_aes)) {
    stop("No colour or fill aes found on this layer!")
  }

  layer$mapping <- utils::modifyList(mapping, list(colour = NULL, fill = NULL))

  params_bleached <- list()
  params_bleached[base::intersect(names(mapping), c("colour", "fill"))] <- unhighlighted_colour
  layer$aes_params <- utils::modifyList(layer$aes_params, params_bleached)

  if (!is.null(group_key)) {
    # To prevent the bleached data to facetted, rename the group column to the very improbable name.
    layer$data <- dplyr::rename(data, !!VERY_SECRET_GROUP_COLUMN_NAME := !!group_key)
    layer$mapping$group <- rlang::quo(!!VERY_SECRET_GROUP_COLUMN_NAME)
  }

  layer
}

sieve_layer <- function(layer, data, mapping, group_key, predicates, max_highlight) {
  if (!is.null(group_key)) {
    data <- dplyr::group_by(data, !! group_key)
  }

  layer$data <- dplyr::filter(data, !!! predicates)
  layer
}

merge_aes <- function(layer_mapping, plot_mapping, aes_names) {
  mapping <- utils::modifyList(plot_mapping %||% aes(), layer_mapping %||% aes())
  aes_names <- base::intersect(aes_names, names(mapping))
  mapping[aes_names]
}
