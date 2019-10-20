check_bad_predicates <- function(x) {
  have_name_idx <- have_name(x)

  if (any(have_name_idx)) {
    bad_x <- x[have_name_idx]
    bad_x_deparsed <- purrr::imap_chr(
      bad_x,
      ~ paste(.y, "=", quo_text(.x))
    )

    abort(
      sprintf(
        "Did you mistype some argument name? Or, did you mean `==`?: %s",
        paste(bad_x_deparsed, collapse = ",")
      )
    )
  }
}

check_bad_label_key <- function(x) {
  if (!quo_is_null(x) && !quo_is_symbol(x)) {
    abort("label_key must be a symbol")
  }
}
