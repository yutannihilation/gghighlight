# Labels

# TODO: return the index of layer so that the group_key can be used
# label_key is a quosure
generate_labelled_layer <- function(layers, label_key) {
  layer_for_label <- choose_layer_for_label(layers, label_key)
  if (is.null(layer_for_label)) {
    return(NULL)
  }

  layer_labelled <- clone_layer(layer_for_label$layer)
  label_layer(layer_labelled, layer_for_label$label)
  layer_labelled
}

choose_layer_for_label <- function(layers, label_key) {
  if (!rlang::quo_is_null(label_key)) {
    # If label_key is specified, some layer must have the kye.
    label_key_text <- rlang::quo_text(label_key)
    idx <- purrr::map_lgl(layers, ~ label_key_text %in% names(.$mapping))
    labellables <- purrr::map(idx, ~ if (.) label_key)
  } else {
    # If label_key is not specified, some key might be usable for label.
    labellables <- purrr::map(layers, ~ infer_label_key(.$mapping))
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
    return(list(layer = layer[idx][[1]], label = labellables[idx][[1]]))
  }
  
  # If there's point geom, use it.
  idx <- purrr::map_lgl(layers, is_identity_point)
  if (any(idx)) {
    return(list(layer = layer[idx][[1]], label = labellables[idx][[1]]))
  }

  # If there's bar geom, use it.
  idx <- purrr::map_lgl(layers, is_bar)
  if (any(idx)) {
    return(list(layer = layer[idx][[1]], label = labellables[idx][[1]]))
  }

  # Other geoms are currently unsupported.
  return(NULL)
}

is_identity_line <- function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomLine")
}

is_identity_point <- function(x) function(x) {
  is_direct_class(x$stat, "StatIdentity") && is_direct_class(x$geom, "GeomPoint")
}

is_bar <- function(x) is_direct_class(x$geom, "GeomBar")

is_direct_class <- function(x, class) identical(class(x)[1], class)

label_layer <- function(layer, label_key) {
  UseGeneric("label_layer")
}

label_layer.default <- function(layer, label_key) stop("Unsupported geom!", call. = FALSE)

label_layer.GeomBar <- function(layer, label_key) NULL

label_layer.GeomPoint <- function(layer, label_key) {
  layer$mapping$label <- label_key
  layer
}

label_layer.GeomLine <- function(layer, label_key) {
  # TODO
}

infer_label_key <- function(layer) {
  # If the layer has label aes, use it.
  if (!is.null(layer$mapping$label)) {
    return(layer$mapping$label)
  }

  # If the layer has groupable aes, use it.
  key <- infer_group_key_from_aes(layer)
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

