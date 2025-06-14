# Utilities

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    rhs
  } else {
    lhs
  }
}

get_facet_vars <- function(facet) {
  switch(
    class(facet)[1],
    "FacetGrid" = as_quosures(c(facet$params$rows, facet$params$cols)),
    "FacetWrap" = as_quosures(facet$params$facets),
    "FacetNull" = NULL,
    abort("Unknown facet class")
  )
}

# Wrap expressions in dplyr::transmute() to prevent from the prior calculated
# results are referred.
quasi_parallel <- function(..., ..nrow) {
  l <- dots_list(..., .named = TRUE)
  if (is_empty(l)) {
    return(NULL)
  }
  tibble::new_tibble(l, nrow = ..nrow)
}

# A simpler version of ggplot2:::make_labels().
# The difference is that this doesn't strip after_stat() or ..
make_label <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }
  if (is_quosure(x) && quo_is_symbol(x)) {
    name <- as_string(quo_get_expr(x))
  } else {
    name <- quo_text(x)
    name <- gsub("\n.*$", "...", name)
  }
  name
}

# return TRUE if
#   - x is NA
#   - x is quo(from_theme(foo %|% NA))
is_na_aes <- function(x) {
  if (is_na(x)) {
    return(TRUE)
  }

  # if X is not NA, inspect inside the quosure

  if (!is_quosure(x)) {
    return(FALSE)
  }

  x <- quo_squash(x)
  if (is_na(x)) {
    return(TRUE)
  }

  if (!is_call(x) || call_name(x) != "from_theme") {
    return(FALSE)
  }

  args <- call_args(x)

  if (
    length(args) == 1L && !is_call(args[[1]]) || call_name(args[[1]]) != "%||%"
  ) {
    return(FALSE)
  }

  args_inner <- call_args(args[[1]])

  if (length(args_inner) != 2L) {
    return(FALSE)
  }

  is_na(args_inner[[2]])
}
