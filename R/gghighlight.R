#' Highlight Data With Predicate
#'
#' @description
#'
#' `gghighlight()` highlights (almost) any geoms according to the given predicates.
#'
#' @name gghighlight
#'
#' @param ...
#'   Expressions to filter data, which is passed to [dplyr::filter()].
#' @param n
#'   Number of layers to clone.
#' @param max_highlight
#'   Max number of series to highlight.
#' @param unhighlighted_params
#'   Aesthetics (e.g. colour, fill, and size) for unhighlighted geoms. Specifying
#'   `colour = NULL` or `fill = NULL` will preserve the original colour.
#' @param use_group_by
#'   If `TRUE`, use [dplyr::group_by()] to evaluate `predicate`.
#' @param use_direct_label
#'   If `TRUE`, add labels directly on the plot instead of using a legend.
#' @param label_key
#'   Column name for `label` aesthetics.
#' @param label_params
#'   A list of parameters, which is passed to [ggrepel::geom_label_repel()].
#' @param keep_scales
#'   If `TRUE`, keep the original data with [ggplot2::geom_blank()] so that the
#'   highlighted plot has the same scale with the data.
#' @param calculate_per_facet
#'   (Experimental) If `TRUE`, include the facet variables to calculate the
#'   grouping; in other words, highlighting is done on each facet individually.
#' @param line_label_type
#'   (Experimental) Method to add labels (or texts) on the highlighted lines.
#'   \describe{
#'     \item{`"ggrepel_label"`}{Use [ggrepel::geom_label_repel()].}
#'     \item{`"ggrepel_text"`}{Use [ggrepel::geom_text_repel()].}
#'     \item{`"text_path"`}{Use [geomtextpath::geom_textline()] for lines and [geomtextpath::geom_textpath()] for paths.}
#'     \item{`"label_path"`}{Use [geomtextpath::geom_labelline()] for lines and [geomtextpath::geom_labelpath()] for paths.}
#'     \item{`"sec_axis"`}{Use secondary axis. Please refer to [Simon Jackson's blog post](https://drsimonj.svbtle.com/label-line-ends-in-time-series-with-ggplot2) for the trick.}
#'   }
#' @param unhighlighted_colour
#'   (Deprecated) Colour for unhighlighted geoms.
#'
#' @examples
#' d <- data.frame(
#'   idx = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   value = c(1, 2, 3, 10, 11, 12, 9, 10, 11),
#'   category = rep(c("a", "b", "c"), 3),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Highlight the lines whose max values are larger than 10
#' ggplot(d, aes(idx, value, colour = category)) +
#'   geom_line() + gghighlight(max(value) > 10)
#'
#' # Highlight the points whose values are larger than 10
#' ggplot(d, aes(idx, value)) +
#'   geom_point() +
#'   gghighlight(value > 10, label_key = category)
#'
#' # Specify the styles for unhighlighted layer
#' ggplot(d, aes(idx, value, colour = category)) +
#'   geom_line(linewidth = 5) +
#'   gghighlight(max(value) > 10,
#'     unhighlighted_params = list(linewidth = 1)
#'   )
#' @export
gghighlight <- function(...,
                        n = NULL,
                        max_highlight = 5L,
                        unhighlighted_params = list(),
                        use_group_by = NULL,
                        use_direct_label = NULL,
                        label_key = NULL,
                        label_params = list(fill = "white"),
                        keep_scales = FALSE,
                        calculate_per_facet = FALSE,
                        line_label_type = c("ggrepel_label", "ggrepel_text", "text_path", "label_path", "sec_axis"),
                        unhighlighted_colour = NULL) {
  predicates <- enquos(...)
  label_key <- enquo(label_key)

  check_bad_predicates(predicates)
  check_bad_label_key(label_key)

  # if fill is not specified, use colour for fill, or vice versa
  unhighlighted_params <- normalize_unhighlighted_params(unhighlighted_params)

  if (!is.null(unhighlighted_colour)) {
    lifecycle::deprecate_warn(
      "0.2.0",
      "gghighlight(unhighlighted_colour = )",
      "gghighlight(unhighlighted_params = )"
    )
    unhighlighted_params$colour <- unhighlighted_colour
  }

  line_label_type <- arg_match(line_label_type)
  if (line_label_type %in% c("text_path", "label_path")) {
    check_installed("geomtextpath")
  }

  # TODO: the default value contains fill, but it's not needed for text
  if (missing(label_params) && line_label_type %in% c("ggrepel_text", "text_path")) {
    label_params <- NULL
  }

  structure(
    list(
      predicates = predicates,
      n = n,
      max_highlight = max_highlight,
      unhighlighted_params = unhighlighted_params,
      use_group_by = use_group_by,
      use_direct_label = use_direct_label,
      line_label_type = line_label_type,
      label_key = label_key,
      label_params = label_params,
      keep_scales = keep_scales,
      calculate_per_facet = calculate_per_facet
    ),
    class = "gg_highlighter"
  )
}

VERY_SECRET_COLUMN_NAME <- sym("highlight..........")

#' @export
ggplot_add.gg_highlighter <- function(object, plot, object_name) {
  if (length(plot$layers) == 0) {
    plot$data <- sieve_data(plot$data, plot$mapping, object$predicates,
      max_highlight = object$max_highlight, use_group_by = object$use_group_by
    )
    return(plot)
  }

  n_layers <- length(plot$layers)
  if (is.null(object$n)) {
    idx_layers <- rep(TRUE, n_layers)
  } else {
    if (object$n > n_layers) {
      abort("n is larger than the actual number of layers!")
    }
    idx_layers <- rep(FALSE, n_layers)
    idx_layers[utils::tail(seq_len(n_layers), object$n)] <- TRUE
  }

  # Layers are environments; if we modify an element in an environment, the
  # modified value is kept. So, we need to clone them before modifying.
  # Note that, since the plot data is overwritten later, we need to attach
  # the data to all layers and then assign back (#31).
  layers_cloned <- purrr::map(plot$layers, clone_layer)

  # Data and group IDs are used commonly both in the bleaching and sieving process
  purrr::walk(layers_cloned, merge_plot_to_layer,
    plot_data = plot$data, plot_mapping = plot$mapping
  )

  # Assign back layers that are not get bleached or sieved
  plot$layers[!idx_layers] <- layers_cloned[!idx_layers]
  layers_cloned <- layers_cloned[idx_layers]

  facet_vars <- if (object$calculate_per_facet) get_facet_vars(plot$facet) else NULL
  group_infos <- purrr::map(
    layers_cloned,
    ~ calculate_group_info(.$data, .$mapping, extra_vars = facet_vars)
  )

  # Clone layers again seperately (layers_cloned will be used later for keeping
  # the original scale). Note that, layers_bleached doesn't need a suffix for 
  # the layer name because this is used for replacing the original layers in
  # place.
  layers_bleached <- purrr::map(layers_cloned, clone_layer)
  layers_sieved <- purrr::map(layers_cloned, clone_layer, layer_name_suffix = "__sieved")

  # Bleach the lower layer.
  purrr::walk2(
    layers_bleached,
    group_infos,
    bleach_layer,
    unhighlighted_colour = default_unhighlighted_colour(plot$theme),
    unhighlighted_params = object$unhighlighted_params,
    calculate_per_facet = object$calculate_per_facet
  )

  # Sieve the upper layer.
  success <- purrr::map2_lgl(
    layers_sieved,
    group_infos,
    sieve_layer,
    predicates = object$predicates,
    max_highlight = object$max_highlight,
    use_group_by = object$use_group_by
  )

  if (!any(success)) {
    abort("All calculations failed! Please provide a valid predicate.")
  }
  if (!all(success)) {
    warn(
      sprintf(
        "Could not calculate the predicate for %s; ignored",
        paste("layer", which(!success), collapse = ", ")
      )
    )
    # remove failed layers
    layers_sieved[!success] <- list(NULL)
  }

  # The plot data should also be sieved to deleting facets for unhighlighted levels
  plot$data <- layers_sieved[[1]]$data

  # skip failed layers
  plot$layers[idx_layers][success] <- layers_bleached[success]

  # Add dummy layers (geom_blank()) to keep the original scales
  if (object$keep_scales) {
    plot <- plot %+% purrr::map(layers_cloned, ~ ggplot2::geom_blank(.$mapping, .$data))
  }

  # 1) use_direct_label is NULL
  #   - try to use direct labels but ignore failures
  #   - when the labels would be too many, do not add labels
  # 2) use_direct_label is TRUE
  #   - use direct labels, otherwise abort()
  #   - even when the labels would be too many, do add labels
  # 3) use_direct_label is FALSE
  #   - do not use direct layers
  if (is_false(object$use_direct_label)) {
    return(plot %+% layers_sieved)
  }

  must_add_labels <- is_true(object$use_direct_label)
  max_labels <- ifelse(must_add_labels, Inf, getOption("gghighlight_max_labels", 20))

  layer_labelled <- generate_labelled_layer(
    layers_sieved, group_infos,
    object$label_key, object$label_params,
    max_labels = max_labels,
    line_label_type = object$line_label_type
  )

  if (is.null(layer_labelled)) {
    if (must_add_labels) {
      abort("No layer can be used for labels")
    }
    return(plot %+% layers_sieved)
  }

  if (object$line_label_type %in% c("text_path", "label_path")) {
    # geomtextpath layers replace the existing line/path layers instead of adding
    # annotation layers.
    plot %+% layer_labelled %+% ggplot2::guides(colour = "none", fill = "none")
  } else if (object$line_label_type %in% c("sec_axis")) {
    # In sec_axis's case, layer_labelled is not a layer in actual; a sec axis object.
    y_scale <- plot$scales$get_scales("y")
    if (is.null(y_scale)) {
      plot <- plot %+% ggplot2::scale_y_continuous(sec.axis = layer_labelled)
    } else {
      if (y_scale$is_discrete()) {
        abort("line_label_type = `sec_axis` cannot be used for discrete scale")
      }
      y_scale$secondary.axis <- layer_labelled
    }

    # do not expand the right side of the X axis
    x_scale <- plot$scales$get_scales("x")
    expand_noright <- ggplot2::expansion(mult = c(0.05, 0))
    if (is.null(x_scale)) {
      plot <- plot %+% ggplot2::scale_x_continuous(expand = expand_noright)
    } else {
      if (!identical(x_scale$expand, ggplot2::waiver())) {
        warn("The `expand` parameter of `scale_x_*()` is ignored and overwritten")
      }
      x_scale$expand <- expand_noright
    }

    plot %+% layers_sieved %+% ggplot2::guides(colour = "none", fill = "none")
  } else {
    plot %+% layers_sieved %+% layer_labelled %+% ggplot2::guides(colour = "none", fill = "none")
  }
}

merge_plot_to_layer <- function(layer, plot_data, plot_mapping) {
  layer$data <- merge_data(layer, plot_data)
  # since gghighlight does grouped calculations, we want to specify the group key by ourselves.
  layer$data <- dplyr::ungroup(layer$data)
  layer$mapping <- merge_mapping(layer, plot_mapping)
  layer
}

merge_data <- function(layer, plot_data) {
  # c.f.) https://github.com/tidyverse/ggplot2/blob/54de616213d9811f422f45cf1a6c04d1de6ccaee/R/layer.r#L182
  layer$layer_data(plot_data)
}

merge_mapping <- function(layer, plot_mapping) {
  # Merge the layer's mapping with the plot's mapping
  mapping <- utils::modifyList(plot_mapping %||% aes(), layer$mapping %||% aes())
  # Filter out unused variables (e.g. fill aes for line geom)
  layer_aes <- base::union(layer$geom$aesthetics(), layer$stat$aesthetics())
  aes_names <- base::intersect(layer_aes, names(mapping))
  mapping <- mapping[aes_names]

  mapping
}

clone_layer <- function(layer, layer_name_suffix = NULL) {
  new_layer <- env_clone(layer)

  # Use a unique name
  if (!is.null(new_layer$name) && !is.null(layer_name_suffix)) {
    new_layer$name <- paste0(new_layer$name, layer_name_suffix)
  }

  class(new_layer) <- class(layer)
  new_layer
}

calculate_group_info <- function(data, mapping, extra_vars = NULL) {
  mapping <- purrr::compact(mapping)
  # the calculation may be possible only in the ggplot2 context (e.g. stat()).
  # So, wrap it with tryCatch() and remove the failed results, which can be
  # detected by checking if all elements are NA or not.
  mapping_wrapped <- purrr::map(mapping, ~ quo(tryCatch(!!., error = function(e) NA)))
  data_evaluated <- dplyr::transmute(data, quasi_parallel(!!!mapping_wrapped, ..nrow = n()))
  data_evaluated <- dplyr::select(data_evaluated, where(~ !all(is.na(.))))

  # As of ggplot2 3.6.0? (TBD), the plot-level mappings is no longer used for the default labels.
  # We need to use the label attribute of each columns instead. cf. https://github.com/tidyverse/ggplot2/pull/5879
  for (nm in colnames(data_evaluated)) {
    attr(data_evaluated[[nm]], "label") <- make_label(mapping[[nm]])
  }

  # Calculate group IDs as ggplot2 does.
  # (c.f. https://github.com/tidyverse/ggplot2/blob/8778b48b37d8b7e41c0f4f213031fb47810e70aa/R/grouping.r#L11-L28)
  if ("group" %in% names(data_evaluated)) {
    group_cols <- "group"
  } else {
    idx_discrete <- purrr::map_lgl(data_evaluated, ~ is.factor(.) || is.character(.) || is.logical(.))
    group_cols <- names(which(idx_discrete))
  }

  # for group key, use symbols only, and don't include extra_vars
  group_keys <- purrr::keep(mapping[group_cols], quo_is_symbol)

  # if extra variables (e.g. facet specs) are specified, use them.
  if (!is.null(extra_vars) && !is_empty(extra_vars)) {
    if (!is_quosures(extra_vars) || !all(have_name(extra_vars))) {
      abort("extra_vars must be a named quosures object.")
    }
    extra_data <- dplyr::transmute(data, quasi_parallel(!!!extra_vars, ..nrow = n()))
  } else {
    extra_data <- NULL
  }

  # no group
  if (length(group_cols) == 0 && is_empty(extra_data)) {
    return(NULL)
  }

  # calculate group IDs with extra_data
  group_df <- dplyr::group_by(
    dplyr::bind_cols(data_evaluated, extra_data),
    across(all_of(c(group_cols, names(extra_data))))
  )
  group_ids <- dplyr::group_indices(group_df)

  list(
    data = data_evaluated,
    id = group_ids,
    key = group_keys
  )
}

bleach_layer <- function(layer, group_info, unhighlighted_params, unhighlighted_colour = default_unhighlighted_colour(), calculate_per_facet = FALSE) {
  # `colour` and `fill` are special in that they needs to be specified even when
  # it is not included in unhighlighted_params. But, if the default_aes is NA,
  # respect it (e.g. geom_bar()'s default colour is NA).
  # Note that this depends on the mapping, so this needs to be done before modifying the mapping.
  for (nm in c("colour", "fill")) {
    if (is.null(unhighlighted_params[[nm]])) {
      # if the aesthetic name is explicit NULL, then do nothing
      if (has_name(unhighlighted_params, nm)) {
        next
      }
      # if the aesthetic name is not specified, fill it with the default
      unhighlighted_params[[nm]] <- get_default_aes_param(nm, layer$geom, layer$mapping, unhighlighted_colour)
    }

    # FIXME: Is this necessary while aes_params will override these mappings?
    # remove colour and fill from mapping
    layer$mapping[nm] <- list(NULL)
  }

  # c.f. https://github.com/tidyverse/ggplot2/blob/e9d4e5dd599b9f058cbe9230a6517f85f3587567/R/layer.r#L107-L108
  aes_params_bleached <- unhighlighted_params[names(unhighlighted_params) %in% layer$geom$aesthetics()]
  geom_params_bleached <- unhighlighted_params[names(unhighlighted_params) %in% layer$geom$parameters(TRUE)]

  layer$aes_params <- utils::modifyList(layer$aes_params, aes_params_bleached)
  layer$geom_params <- utils::modifyList(layer$geom_params, geom_params_bleached)

  # FIXME: can this be removed?
  if (is.null(group_info$key)) {
    return(layer)
  }

  # In order to prevent the bleached layer from being facetted, we need to rename
  # columns of group keys to improbable names. But, what happens when the group
  # column disappears? Other calculations that uses the column fail. So, we need
  # to use the pre-evaluated values and rename everything to improbable name.
  bleached_data <- group_info$data
  secret_prefix <- expr_text(VERY_SECRET_COLUMN_NAME)
  mapping_names <- names(bleached_data)
  secret_names <- paste0(secret_prefix, seq_along(mapping_names))
  secret_quos <- quos(!!!syms(secret_names))
  bleached_data <- dplyr::rename(bleached_data, !!!stats::setNames(mapping_names, secret_names))
  layer$mapping <- utils::modifyList(layer$mapping, stats::setNames(secret_quos, mapping_names))

  secret_name_group <- paste0(secret_prefix, "group")
  bleached_data[secret_name_group] <- factor(group_info$id)
  layer$mapping$group <- quo(!!sym(secret_name_group))

  # FIXME:
  # Contradictorily to the comment above, we need the original data to let the
  # layer be facetted. Probably, we can make here more efficient...
  if (calculate_per_facet) {
    layer$data <- dplyr::bind_cols(bleached_data, layer$data)
  } else {
    layer$data <- bleached_data
  }

  layer
}


default_unhighlighted_colour <- function(theme = list()) {
  geom <- theme$geom
  if (utils::packageVersion("ggplot2") <= "3.5.1" || is.null(geom)) {
    return("#BEBEBEB2")
  }

  # cf. ggplot2:::col_mix
  ink <- grDevices::col2rgb(geom$ink %||% "black")
  paper <- grDevices::col2rgb(geom$paper %||% "white")
  # 0.745098 = col2rgb("grey") / col2rgb("white")
  new <- (0.254902 * paper + 0.745098 * ink)[,1]
  grDevices::rgb(new["red"], new["green"], new["blue"], alpha = 179, maxColorValue = 255)
}

get_default_aes_param <- function(aes_param_name, geom, mapping, unhighlighted_colour) {
  # bleach only colour and fill
  if (!aes_param_name %in% c("colour", "fill")) {
    return(NULL)
  }

  # if it is specified in mapping, it needs to be overriden
  if (aes_param_name %in% names(mapping)) {
    unhighlighted_colour
  }

  non_null_default_aes <- purrr::compact(geom$default_aes)

  # if the geom has default value and is NA, use NA
  if (aes_param_name %in% names(non_null_default_aes) &&
    # is.na() cannot handle non primitive objects (e.g. a quosure)
    rlang::is_na(non_null_default_aes[[aes_param_name]])) {
    return(NA)
  }

  unhighlighted_colour
}

sieve_layer <- function(layer, group_info, predicates,
                        max_highlight = 5L,
                        use_group_by = NULL) {
  # If there are no predicates, do nothing.
  if (length(predicates) == 0) return(TRUE)

  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)

  # 1) If use_group_by is FALSE, do not use group_by().
  # 2) If use_group_by is TRUE and group IDs don't exist, use group_by().
  # 3) If use_group_by is TRUE but group IDs exist, show a warning and do not use group_by().
  if (use_group_by) {
    if (is.null(group_info$id)) {
      msg <- paste0(
        "Tried to calculate with group_by(), but there seems no groups.\n",
        "Please provide group, colour or fill aes.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)

      use_group_by <- FALSE
    }
  }

  names(predicates) <- paste0("p", seq_along(predicates))

  # If use_group_by is TRUE, try to calculate grouped
  if (use_group_by) {
    tryCatch({
      layer$data <- calculate_grouped(layer$data, predicates, max_highlight, group_info$id)
      # if this succeeds, return TRUE
      return(TRUE)
    },
    error = function(e) {
      msg <- paste0(
        "Tried to calculate with group_by(), but the calculation failed.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)
    }
    )
  }

  # the grouped calculation failed or skipped, try ungrouped one.
  tryCatch({
    layer$data <- calculate_ungrouped(layer$data, predicates, max_highlight)
    return(TRUE)
  },
  error = function(e) {
    # do not warn here, but in ggplot_add.gg_highlighter()
    return(FALSE)
  })

  FALSE
}

# TODO: integrate with sieve_layer
sieve_data <- function(data, mapping, predicates, group_info = NULL,
                       max_highlight = 5L, use_group_by = TRUE) {
  if (is.null(group_info)) {
    group_info <- calculate_group_info(data, mapping)
  }

  # If use_group_by is NULL, infer it from whether group_key is NULL or not.
  use_group_by <- use_group_by %||% !is.null(group_info$id)

  if (use_group_by) {
    tryCatch({
      return(calculate_grouped(data, predicates, max_highlight, group_info$id))
    },
    error = function(e) {
      msg <- paste0(
        "Tried to calculate with group_by(), but the calculation failed.\n",
        "Falling back to ungrouped filter operation..."
      )
      warn(msg)
    })
  }

  return(calculate_ungrouped(data, predicates, max_highlight))
}

calculate_grouped <- function(data, predicates, max_highlight, group_ids) {
  data_predicated <- data

  data_predicated <- dplyr::group_by(data_predicated, !!VERY_SECRET_COLUMN_NAME := !!group_ids)
  data_predicated <- dplyr::summarise(data_predicated, quasi_parallel(!!!predicates, ..nrow = 1L))

  group_with_multiple_result <- anyDuplicated(dplyr::pull(data_predicated, !!VERY_SECRET_COLUMN_NAME))
  if (!identical(group_with_multiple_result, 0L)) {
    abort("", "gghighlight_invalid_group_predicate_error")
  }

  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)

  # Filter by the logical predicates.
  data_filtered <- dplyr::filter(data_predicated, if_all(all_of(cols$filter)))

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- dplyr::arrange(data_filtered, across(all_of(cols$arrange)))
    data_filtered <- utils::tail(data_filtered, max_highlight)
  }

  groups_filtered <- dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME)

  data[group_ids %in% groups_filtered, , drop = FALSE]
}

calculate_ungrouped <- function(data, predicates, max_highlight) {
  data_predicated <- data

  data_predicated <- tibble::rowid_to_column(data_predicated, var = expr_text(VERY_SECRET_COLUMN_NAME))
  data_predicated <- dplyr::transmute(
    data_predicated,
    quasi_parallel(!!!predicates, ..nrow = n()),
    !!VERY_SECRET_COLUMN_NAME
  )

  cols <- choose_col_for_filter_and_arrange(data_predicated, VERY_SECRET_COLUMN_NAME)

  # Filter by the logical predicates.
  data_filtered <- dplyr::filter(data_predicated, if_all(all_of(cols$filter)))

  # Arrange by the other predicates and slice rows down to max_highlights.
  if (length(cols$arrange) > 0) {
    data_filtered <- dplyr::filter(data_filtered, if_all(cols$arrange, ~ !is.na(.)))
    data_filtered <- dplyr::arrange(data_filtered, across(all_of(cols$arrange)))
    data_filtered <- utils::tail(data_filtered, max_highlight)
  }

  # sort to preserve the original order
  rowids_filtered <- sort(dplyr::pull(data_filtered, !!VERY_SECRET_COLUMN_NAME))

  data[rowids_filtered, , drop = FALSE]
}

choose_col_for_filter_and_arrange <- function(data, exclude_col) {
  # Do not use row IDs or group keys to arrange.
  data <- dplyr::select(data, -!!exclude_col)
  col_idx_lgl <- purrr::map_lgl(data, is.logical)
  col_idx_lst <- purrr::map_lgl(data, is.list)
  list(
    # Use logical columns for filter()
    filter = names(data)[col_idx_lgl],
    # Use other columns but lists for arrange() (arrange doesn't support list columns)
    arrange = names(data)[!col_idx_lgl & !col_idx_lst]
  )
}

normalize_unhighlighted_params <- function(aes_params) {
  if (!is.list(aes_params)) {
    abort("unhighlighted_params must be a list.")
  }

  # color is an alias of colour
  if (!is.null(aes_params$color)) {
    aes_params$colour <- aes_params$colour %||% aes_params$color
    aes_params$color <- NULL
  }

  aes_params
}
