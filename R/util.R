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
    "FacetGrid" = rlang::as_quosures(c(facet$params$rows, facet$params$cols)),
    "FacetWrap" = rlang::as_quosures(facet$params$facets),
    "FacetNull" = NULL,
    rlang::abort("Unknown facet class")
  )
}
