#' Highlight With Predicates
#'
#' @param .predicate an expression to be used as a predicate for highlight
#' @param ... other argument passed to `highlight_scale()`
#' @export
scale_highlight_colour <- function(.predicate, ...) {
  highlight_scale("colour", "brewer", predicate = rlang::enquo(.predicate), ...)
}

#' ggproto for Highlight
#'
#' @name ScaleHighlight
#' @format NULL
#' @usage NULL
#' @export
ScaleHighlight <- ggplot2::ggproto("Scale", ggplot2::ScaleDiscrete,
  # a quosure predicate
  predicate = NULL,
  # a named logical vector whether to highlight the key
  highlight = NULL,
  train_df = function(self, df) {
    # dirty hack
    index_lapply <- rlang::caller_env(5)$i
    df_raw <- rlang::caller_env(6)$layer_data[[index_lapply]]
    mapping <- unclass(rlang::caller_env(6)$layers[[index_lapply]]$mapping)

    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()
    if (length(self$aesthetics) > 1) stop("I don't know how to handle more than two aesthetics", self$aesthetics)

    aes_name <- mapping[[self$aesthetics]]
    aes_chr <- as.character(aes_name)

    gdf <- dplyr::group_by(df_raw, !! aes_name)
    sdf <- dplyr::summarise(gdf, result = !! self$predicate)
    self$highlight <- rlang::set_names(sdf$result, sdf[[aes_chr]])

    ggplot2::ggproto_parent(ggplot2::ScaleDiscrete, self)$train_df(df)
  },
  map = function(self, x, limits = self$get_limits()) {
    # TODO: use palette
    pal_match <- dplyr::if_else(self$highlight[x],
                                ggplot2::alpha("red", 1),
                                ggplot2::alpha("grey", 0.5))

    # TODO: handle NAs
    if (self$na.translate) {
      ifelse(is.na(x) | is.na(pal_match), self$na.value, pal_match)
    } else {
      pal_match
    }
  },
  make_title = function(self, title) {
    sprintf('%s\n(highlight: %s)', title, rlang::quo_text(self$predicate))
  }
)

highlight_scale <- function(aesthetics,
                            scale_name,
                            guide = "legend",
                            position = "left",
                            predicate = NULL) {

  position <- match.arg(position, c("left", "right", "top", "bottom"))
  scale_obj <- ggplot2::discrete_scale(aesthetics,
                                       scale_name,
                                       scales::identity_pal(),   # TODO: use palette
                                       guide = "legend",
                                       position = "left",
                                       super = ScaleHighlight)

  scale_obj$predicate <- predicate
  scale_obj
}
