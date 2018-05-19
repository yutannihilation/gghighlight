#' Clone Other Layers
#'
#' @param n
#'   Number of layers to clone.
#' @inheritParams ggplot2::geom_bar
#' @export
geom_highlight <- function(...,
                           n = NULL,
                           unhighlighted_colour = ggplot2::alpha("grey", 0.7)) {
  structure(
    list(
      predicate = rlang::enquos(...),
      n = n,
      unhighlighted_colour = unhighlighted_colour
    ),
    class = "gg_highlighter"
  )
}

#' @export
ggplot_add.gg_highlighter <- function(object, plot, object_name) {
  if (length(plot$layers) == 0) {
    stop("there is no layer to highlight!")
  }

  if (!is.null(object$n)) {
    layers_bleached <- utils::tail(plot$layers, object$n)
  } else {
    layers_bleached <- plot$layers
  }

  layers_highlighted <- purrr::map(layers_bleached, clone_layer)

  purrr::walk(layers_bleached, bleach_layer,
              plot_mapping = plot$mapping,
              unhighlighted_colour = object$unhighlighted_colour)

  purrr::walk(layers_highlighted, highlight_layer)

  plot %+% layers_highlighted
}

bleach_layer <- function(layer, plot_mapping, unhighlighted_colour) {
  mapping <- merge_aes(layer$mapping, plot_mapping, layer$geom$aesthetics())

  if (length(mapping) == 0) {
    stop("No mapping found on this layer!")
  }

  colour_aes <- mapping$colour
  fill_aes <- mapping$fill

  if (is.null(colour_aes) && is.null(fill_aes)) {
    stop("No colour or fill aes found on this layer!")
  }

  group_aes <- infer_group_key_from_aes(mapping)

  layer$mapping <- utils::modifyList(layer$mapping, list(group = group_aes, colour = NULL, fill = NULL))

  params_bleached <- list()
  params_bleached[base::intersect(names(mapping), c("colour", "fill"))] <- unhighlighted_colour
  layer$aes_params <- utils::modifyList(layer$aes_params, params_bleached)

  layer
}

merge_aes <- function(layer_mapping, plot_mapping, aes_names) {
  mapping <- utils::modifyList(plot_mapping %||% aes(), layer_mapping %||% aes())
  aes_names <- base::intersect(aes_names, names(mapping))
  mapping[aes_names]
}

# ggplot() generates empty aes, whereas geom_*() generates NULL aes
is_null_or_empty_aes <- function(x) {
  is.null(x) || length(x) == 0
}
