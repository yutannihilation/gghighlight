#' Highlight With Predicates
#'
#' @export
scale_highlight_colour <- function(.predicate, ..., type = "seq", palette = 1, direction = 1) {
  highlight_scale("colour", "brewer", scales::brewer_pal(type, palette, direction), predicate = rlang::enquo(.predicate), ...)
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
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()
    if (length(self$aesthetics) > 1) stop("I don't know how to handle more than two aesthetics", self$aesthetics)

    gdf <- dplyr::group_by(df, !! rlang::sym(self$aesthetics))
    sdf <- dplyr::summarise(gdf, result = !! self$predicate)
    self$highlight <- rlang::set_names(sdf$result, sdf[[self$aesthetics]])

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

# TODO: do not copy and paste codes
highlight_scale <- function(aesthetics, scale_name, palette, name = ggplot2::waiver(),
  breaks = ggplot2::waiver(), labels = ggplot2::waiver(), limits = NULL, expand = ggplot2::waiver(),
  na.translate = TRUE, na.value = NA, drop = TRUE,
  guide = "legend", position = "left", super = ScaleHighlight,
  predicate = NULL) {

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  ggproto(NULL, super,
          call = match.call(),

          aesthetics = aesthetics,
          scale_name = scale_name,
          palette = palette,

          range = ggplot2:::discrete_range(),
          limits = limits,
          na.value = na.value,
          na.translate = na.translate,
          expand = expand,

          name = name,
          breaks = breaks,
          labels = labels,
          drop = drop,
          guide = guide,
          position = position,
          predicate = predicate
  )
}
