# Labels

generate_labelled_layer <- function(layers, group_keys, label_key) {
  layer_for_label <- choose_layer_for_label(layers, group_keys, label_key)
  if (is.null(layer_for_label)) {
    return(NULL)
  }

  layer_labelled <- clone_layer(layer_for_label$layer)
  label_layer(layer_labelled, layer_for_label$group_key, layer_for_label$label)
}

choose_layer_for_label <- function(layers, group_keys, label_key) {
  if (!rlang::quo_is_null(label_key)) {
    # If label_key is specified, some layer must have the kye.
    label_key_text <- rlang::quo_text(label_key)
    idx <- purrr::map_lgl(layers, ~ label_key_text %in% names(.$mapping))
    labellables <- purrr::map(idx, ~ if (.) label_key)
  } else {
    # If label_key is not specified, some key might be usable for label.
    labellables <- purrr::map2(layers, group_keys, ~ infer_label_key(.x$mapping, .y))
    idx <- !purrr::map_lgl(labellables, is.null)
  }

  # If no layer has labellable variable, give up labelling
  if (!any(idx)) {
    return(NULL)
  }

  # Filter out the layers that cannot be labelled.
  layers <- layers[idx]
  labellables <- labellables[idx]
  group_keys <- group_keys[idx]

  # If there's line geom, use it.
  idx <- purrr::map_lgl(layers, is_identity_line)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], group_key = group_keys[[idx]], label = labellables[idx][[1]]))
  }

  # If there's point geom, use it.
  idx <- purrr::map_lgl(layers, is_identity_point)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], group_key = group_keys[[idx]], label = labellables[idx][[1]]))
  }

  # If there's bar geom, use it.
  idx <- purrr::map_lgl(layers, is_bar)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], group_key = group_keys[[idx]], label = labellables[idx][[1]]))
  }

  # Other geoms are currently unsupported.
  return(NULL)
}

is_identity_line <- function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomLine")
}

is_identity_point <- function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomPoint")
}

is_bar <- function(x) is_direct_class(x$geom, "GeomBar")

is_direct_class <- function(x, class) identical(class(x)[1], class)

label_layer <- function(layer, group_key, label_key) {
  switch (class(layer$geom)[1],
    GeomLine = label_layer_line(layer, group_key, label_key),
    GeomPoint = label_layer_point(layer, group_key, label_key),
    # TODO: To distinguish NULL, return list() to hide guides here.
    #       But, can we use more explicit representation?
    GeomBar = list(),
    stop("Unsupported geom!", call. = FALSE)
  )
}

label_layer_point <- function(layer, group_key, label_key) {
  mapping <- layer$mapping
  mapping$label <- label_key

  ggrepel::geom_label_repel(data = layer$data,
                            mapping = mapping)
}

label_layer_line <- function(layer, group_key, label_key) {
  mapping <- layer$mapping
  mapping$label <- label_key

  x_key <- layer$mapping$x

  rightmost_points <- layer$data %>%
    dplyr::group_by(!!group_key) %>%
    dplyr::filter(!!x_key == max(!!x_key)) %>%
    dplyr::slice(1)

  ggrepel::geom_label_repel(data = rightmost_points,
                            mapping = mapping)
}

infer_label_key <- function(layer, group_key) {
  # If the layer has label aes or group_key, use it.
  key <- layer$mapping$label %||% group_key
  if (!is.null(key)) {
    return(key)
  }

  # If the data has discrete variable, use it.
  idx <- purrr::map_lgl(layer$data, is.character) | purrr::map_lgl(layer$data, is.factor)
  if (any(idx)) {
    col_sym <- rlang::sym(colnames(layer$data)[idx][[1]])
    return(rlang::quo(!!col_sym))
  }

  return(NULL)
}

