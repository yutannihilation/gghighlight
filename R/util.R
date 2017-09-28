# Utilities

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    rhs
  } else {
    lhs
  }
}
