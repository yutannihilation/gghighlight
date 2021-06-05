# Utilities

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    rhs
  } else {
    lhs
  }
}

get_facet_vars <- function(facet) {
  switch(class(facet)[1],
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
