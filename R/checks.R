check_bad_predicates <- function(x) {
  have_name_idx <- rlang::have_name(x)

  if (any(have_name_idx)) {
    bad_x <- x[have_name_idx]
    bad_x_deparsed <- purrr::imap_chr(
      bad_x,
      ~ paste(.y, "=", rlang::quo_text(.x))
    )

    rlang::abort(
      sprintf("Did you mistyped `==` as `=`?: %s",
              paste(bad_x_deparsed, collapse = ","))
    )
  }
}

check_bad_label_key <- function(x) {
  if (!rlang::quo_is_null(x) && !rlang::quo_is_symbol(x)) {
    rlang::abort("label_key must be a symbol")
  }
}
