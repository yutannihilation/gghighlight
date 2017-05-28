#' Highlight With Predicates
#'
#' @name scale_highlight
#' @param .predicate an expression to be used as a predicate for highlight
#' @param ... other argument passed to `highlight_scale()`
#' @param .default_colour colour for unhighlighted elements
#' @export
scale_highlight_colour <- function(.predicate, ..., .default_colour = NULL) {
  highlight_scale("colour", "brewer",
                  predicate = rlang::enquo(.predicate),
                  .default_colour = .default_colour,
                  ...)
}

#' @name scale_highlight
#' @export
scale_highlight_color <- scale_highlight_colour

#' @name scale_highlight
#' @export
scale_highlight_fill <- function(.predicate, ..., .default_colour = NULL) {
  highlight_scale("fill", "brewer",
                  predicate = rlang::enquo(.predicate),
                  .default_colour = .default_colour,
                  ...)
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

  .default_colour = ggplot2::alpha("grey", 0.2),

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
    # something is wrong if this doesn't match
    if (length(limits) != length(self$highlight)) stop('limits and highlights has different length')

    n <- sum(!is.na(limits))
    if (!is.null(self$n.breaks.cache) && self$n.breaks.cache == n) {
      pal <- self$palette.cache
    } else {
      if (!is.null(self$n.breaks.cache)) warning("Cached palette does not match requested", call. = FALSE)
      # default value for non-highlited elements
      pal <- rep(self$.default_colour, length(limits))
      names(pal) <- limits
      # assign colour if it should be highlighted
      pal[self$highlight[limits]] <- self$palette(sum(self$highlight))

      self$palette.cache <- pal
      self$n.breaks.cache <- n
    }

    if (is.null(names(pal))) {
      pal_match <- pal[match(as.character(x), limits)]
    } else {
      pal_match <- pal[match(as.character(x), names(pal))]
      pal_match <- unname(pal_match)
    }

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
                            predicate = NULL,
                            .default_colour = NULL) {

  position <- match.arg(position, c("left", "right", "top", "bottom"))
  scale_obj <- ggplot2::discrete_scale(aesthetics,
                                       scale_name,
                                       viridis::viridis_pal(option = "inferno", end = 0.7),
                                       guide = "legend",
                                       position = "left",
                                       super = ScaleHighlight)

  scale_obj$predicate <- predicate
  if (!is.null(.default_colour)) scale_obj$.default_colour <- .default_colour
  scale_obj
}
