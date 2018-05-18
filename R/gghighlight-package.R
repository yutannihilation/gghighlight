#' Highlight Lines and Points in 'ggplot2'
#'
#' Make it easier to explore data with highlights.
#'
#' @name gghighlight-package
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom ggplot2 %+%
#' @docType package
NULL

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

.onLoad <- function(libname, pkgname) {
  register_s3_method("ggplot2", "ggplot_add", "gg_cloner")
}

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
