# Labels

generate_labelled_layer <- function(layers, group_keys, label_key) {
  layer_for_label <- choose_layer_for_label(layers, group_keys, label_key)
  if (is.null(layer_for_label)) {
    return(NULL)
  }

  layer <- layer_for_label$layer
  label_key <- layer_for_label$label_key

  switch (class(layer$geom)[1],
    GeomLine = generate_label_for_line(layer, label_key),
    GeomPoint = generate_label_for_point(layer, label_key),
    # TODO: To distinguish NULL, return list() to hide guides here.
    #       But, can we use more explicit representation?
    GeomBar = list(),
    stop("Unsupported geom!", call. = FALSE)
  )
}

choose_layer_for_label <- function(layers, group_keys, label_key) {
  if (!rlang::quo_is_null(label_key)) {
    # If label_key is specified, some layer must have the key in their data.
    label_key_text <- rlang::quo_text(label_key)
    idx <- purrr::map_lgl(layers, ~ label_key_text %in% names(.$data))
    labellables <- purrr::map(idx, ~ if (.) label_key)
  } else {
    # If label_key is not specified, some key might be usable for label.
    labellables <- purrr::map2(layers, group_keys, infer_label_key)
    idx <- !purrr::map_lgl(labellables, is.null)
  }

  # If no layer has labellable variable, give up labelling
  if (!any(idx)) {
    return(NULL)
  }

  # Filter out the layers that cannot be labelled.
  layers <- layers[idx]
  labellables <- labellables[idx]

  # If there's line geom, use it.
  idx <- purrr::map_lgl(layers, is_identity_line)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], label_key = labellables[idx][[1]]))
  }

  # If there's point geom, use it.
  idx <- purrr::map_lgl(layers, is_identity_point)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], label_key = labellables[idx][[1]]))
  }

  # If there's bar geom, use it.
  idx <- purrr::map_lgl(layers, is_bar)
  if (any(idx)) {
    return(list(layer = layers[idx][[1]], label_key = labellables[idx][[1]]))
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

generate_label_for_line <- function(layer, label_key) {
  mapping <- layer$mapping
  mapping$label <- label_key

  x_key <- layer$mapping$x
  group_key <- layer$mapping$group %||% layer$mapping$colour

  # To restore the original group, extract group keys (I don't know this is really necessary, though...)
  group_key_orig <- dplyr::groups(layer$data)

  rightmost_points <- layer$data %>%
    dplyr::group_by(!!group_key) %>%
    dplyr::filter(!!x_key == max(!!x_key)) %>%
    # max value can appear multiple times, so ensure only one row per group
    dplyr::slice(1)

  # restore the original group
  rightmost_points <- dplyr::group_by(rightmost_points, !!!group_key_orig)

  ggrepel::geom_label_repel(mapping, rightmost_points)
}

generate_label_for_point <- function(layer, label_key) {
  mapping <- layer$mapping
  mapping$label <- label_key

  ggrepel::geom_label_repel(mapping, layer$data)
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

