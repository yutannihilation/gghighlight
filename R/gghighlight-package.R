#' Highlight Lines and Points in 'ggplot2'
#'
#' Make it easier to explore data with highlights.
#'
#' @name gghighlight-package
#' @import rlang
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 ggplot_add
#' @importFrom dplyr desc
#' @docType package
NULL

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

# TODO: where() is not exported from tidyselect.
#       Fix this when r-lib/tidyselect#201 is closed.
utils::globalVariables("where")
