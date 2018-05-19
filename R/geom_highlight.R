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

  group_keys <- purrr::map(layers_bleached, c("mapping", "group"))

  purrr::walk2(layers_highlighted, group_keys, sieve_layer,
               plot_data = plot$data,
               predicates = object$predicates,
               max_highlight = object$max_highlight)

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

  group_key <- infer_group_key_from_aes(mapping)

  layer$mapping <- utils::modifyList(mapping, list(group = group_key, colour = NULL, fill = NULL))

  params_bleached <- list()
  params_bleached[base::intersect(names(mapping), c("colour", "fill"))] <- unhighlighted_colour
  layer$aes_params <- utils::modifyList(layer$aes_params, params_bleached)

  layer
}

sieve_layer <- function(layer, plot_data, predicates, group_key, max_highlight) {
  # c.f.) https://github.com/tidyverse/ggplot2/blob/54de616213d9811f422f45cf1a6c04d1de6ccaee/R/layer.r#L182
  data <- layer$layer_data(plot_data)

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
