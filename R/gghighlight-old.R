#' Highlight Data With Predicate
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("defunct")}
#'
#' `gghiglight_line()` and `gghighlight_point()` are deprecated. Please use [gghighlight()] instead.
#'
#' @name gghighlight-old
#'
#' @param ... Ignored.
#'
#' @keywords internal
NULL

#' @rdname gghighlight-old
#' @export
gghighlight_line <- function(...) {
  lifecycle::deprecate_stop("0.1.0", "gghighlight_line()", with = "gghighlight()")
}


#' @rdname gghighlight-old
#' @export
gghighlight_point <- function() {
  lifecycle::deprecate_stop("0.1.0", "gghighlight_point()", with = "gghighlight()")
}
