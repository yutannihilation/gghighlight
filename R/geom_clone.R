#' Clone Other Layers
#'
#' @name clone
#' @export
geom_clone <- function(mapping = NULL,
                       data = NULL,
                       n = NULL,
                       ...) {
  structure(
    list(
      mapping = mapping,
      data = data,
      n = n,
      aes_params = list(...)
    ),
    class = "gg_cloner"
  )
}

ggplot_add.gg_cloner <- function(object, plot, object_name) {
  if (!is.null(object$n)) {
    layers_to_clone <- tail(plot$layers, object$n)
  } else {
    layers_to_clone <- plot$layers
  }

  new_layers <- purrr::map(layers_to_clone, clone_layer)
  new_layers <- purrr::map(new_layers, update_layer,
                           data = object$data,
                           mapping = object$mapping,
                           aes_params = object$aes_params)

  plot + new_layers
}

clone_layer <- function(layer) {
  new_layer <- rlang::env_clone(layer)
  class(new_layer) <- class(layer)
  layer
}

update_layer <- function(layer, data, mapping, aes_params) {
  layer$data <- data %||% layer$data
  layer$mapping <- rename_variables(mapping) %||% layer$mapping
  layer$aes_params <- modifyList(layer$aes_params, rename_variables(aes_params))
  layer
}

rename_variables <- function(mapping) {
  if (is.null(mapping)) return(NULL)

  conversion_table <- c(color = "colour")
  names_orig <- names(mapping)
  names(mapping) <- dplyr::coalesce(conversion_table[names_orig], names_orig)
  mapping
}
