#' Highlight Lines and Points in 'ggplot2'
#'
#' Make it easier to explore data with highlights.
#'
#' @name gghighlight-package
#' @import rlang
#' @importFrom ggplot2 ggplot_add
#' @importFrom dplyr desc across if_all all_of
"_PACKAGE"

#' @importFrom ggplot2 aes
#' @export
ggplot2::aes

# TODO: where() is not exported from tidyselect.
#       Fix this when r-lib/tidyselect#201 is closed.
utils::globalVariables("where")

#' @importFrom dplyr n
#' @export
dplyr::n

# TODO: remove this when I remove support for ggplot2 v3
`%+%` <- function(e1, e2) {
  if (utils::packageVersion("ggplot2") > "3.5.2.9000") {
    ggplot2::add_gg(e1, e2)
  } else {
    ggplot2::`%+%`(e1, e2)
  }
}
