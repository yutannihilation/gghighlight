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
  predicate = NULL,
  map_df = function(self, df, i = NULL) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()

    gdf <- dplyr::group_by(df, group)
    x <- dplyr::transmute(gdf, result = !! self$predicate)
    list(
      colour = dplyr::if_else(x$result, "red", "grey"),
      alpha  = dplyr::if_else(x$result, 1, 0.5)
    )
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
