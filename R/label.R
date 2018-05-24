# Labels

# TODO: return the index of layer so that the group_key can be used
# label_key is a quosure
choose_layer_for_label <- function(layers, label_key) {
  if (!rlang::quo_is_null(label_key)) {
    label_key_text <- rlang::quo_text(label_key)
    layers <- purrr::keep(layers, ~ label_key_text %in% names(.$mapping))
    if (length(layers) == 0) {
      warning("No layer has the variable for label: ", label_key_text, call. = FALSE)
      return(NULL)
    }
  }

  layer_identity_line <- purrr::keep(layers, is_identity_line)
  if (length(layer_identity_line) > 0) {
    return(layer_identity_line[[1]])
  }

  layer_identity_point <- purrr::keep(layers, is_identity_point)
  if (length(layer_identity_point) > 0) {
    return(layer_identity_point[[1]])
  }

  layer_bar <- purrr::keep(layers, is_bar)
  if (length(layer_bar) > 0) {
    return(layer_bar[[1]])
  }

  return(NULL)
}

is_identity_line <- function(x) {
  identical(is(x$stat), "StatIdentity") && identical(is(x$geom), "GeomLine")
}

is_identity_point <- function(x) function(x) {
  identical(is(x$stat), "StatIdentity") && identical(is(x$geom), "GeomPoint")
}

is_bar <- function(x) identical(is(x$geom), "GeomBar")

generate_layer_label <- function(layer_labelled, label_key) {
  if (is.null(label_key)) {
    label_key <- infer_label_key(layer_labelled)
  }
}

infer_label_key <- function(layer) {

}
