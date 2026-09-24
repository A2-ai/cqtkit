#' Add Error Bars To Plot
#'
#' Adds errorbars to a plot.
#'
#' @param data A data frame from compute_grouped_mean_sd
#' @param p A ggplot object to add error bars to
#' @param reference_dose Reference dose value for comparison calculations
#' @param error_bars Type of errorbars to use (ci, se, sd, null)
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @return A ggplot2 object with error bars added
add_error_bars_to_plot <- function(
  data,
  p,
  reference_dose,
  error_bars,
  conf_int
) {
  caption <- ""
  if (!is.null(error_bars)) {
    bounds <- if (is.null(reference_dose)) {
      switch(error_bars,
        CI = ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high),
        SE = ggplot2::aes(
          ymin = .data$mean_dv - .data$se,
          ymax = .data$mean_dv + .data$se
        ),
        SD = ggplot2::aes(
          ymin = .data$mean_dv - .data$sd,
          ymax = .data$mean_dv + .data$sd,
          y = .data$mean_dv
        )
      )
    } else {
      switch(error_bars,
        CI = ggplot2::aes(ymin = .data$ci_low_delta, ymax = .data$ci_up_delta),
        SE = ggplot2::aes(
          ymin = .data$mean_delta_dv - .data$delta_se,
          ymax = .data$mean_delta_dv + .data$delta_se
        ),
        SD = ggplot2::aes(
          ymin = .data$mean_delta_dv - .data$delta_sd,
          ymax = .data$mean_delta_dv + .data$delta_sd
        )
      )
    }

    p <- p + ggplot2::geom_errorbar(data = data, mapping = bounds)
    caption <- if (error_bars == "CI") {
      paste0("errorbars represent ", round(conf_int * 100), "% CI")
    } else {
      paste("errorbars represent", error_bars)
    }
  }

  #should add label/annotation for thresholds if they aren't null...
  p <- p +
    ggplot2::labs(
      caption = paste(caption, "\n")
    )

  return(p)
}

# A named line (reference, regression, prediction or VPC percentile) keyed
# into the linetype legend. ggstylekit styles it by name; the list path reads
# `cqtkit_series` to apply `colors` to it, since the layer maps no colour.
line_series_layer <- function(layer, name) {
  linetype <- layer$aes_params$linetype %||% "solid"
  layer <- ggstylekit::series_layer(layer, name, legend_channel = "linetypes")
  attr(layer, "cqtkit_series") <- name
  attr(layer, "cqtkit_linetype") <- linetype
  layer
}

# Adds to a summary the columns of `data` that have one value in every summary
# cell (a study nested in the dose groups, say), so `reveal()` can map them.
# The summary's own columns and the time and dv columns are skipped. The dose
# and group columns are cell keys, so they are always carried.
carry_constant_columns <- function(summary, data, time, dose, group, dv) {
  used <- unlist(lapply(c(time, dv), name_quo_if_not_null))
  candidates <- setdiff(names(data), c(names(summary), used))
  if (length(candidates) == 0) {
    return(summary)
  }

  cells <- tibble::tibble(
    time = dplyr::pull(data, !!time),
    dose = dplyr::pull(data, !!dose)
  )
  if (!rlang::quo_is_null(group)) {
    cells$group <- paste_grouping(cells$dose, complete_group_values(data, group))
  }
  keys <- names(cells)
  cells <- dplyr::bind_cols(cells, data[candidates])

  constant <- cells |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(candidates), ~ dplyr::n_distinct(.x) == 1),
      .groups = "drop"
    )
  keep <- candidates[vapply(candidates, function(x) all(constant[[x]]), logical(1))]
  if (length(keep) == 0) {
    return(summary)
  }

  values <- dplyr::distinct(cells, dplyr::across(dplyr::all_of(c(keys, keep))))
  dplyr::left_join(summary, values, by = keys)
}

# Reference lines on the colour channel, for plots whose points map colour
# only, so the references join the colour legend. Sets the default colours
# (predictions and references black) the styling reads from `series_colors`.
add_color_references <- function(p, reference_threshold) {
  attr(p, "series_colors") <- c("Predictions" = "black")
  if (length(reference_threshold) == 0) {
    return(p)
  }

  ref <- data.frame(
    yintercept = reference_threshold,
    group = paste0("Reference ", reference_threshold)
  )
  p <- p +
    ggplot2::geom_hline(
      data = ref,
      ggplot2::aes(yintercept = .data$yintercept, color = .data$group),
      inherit.aes = FALSE,
      linetype = "dashed"
    )
  attr(p, "series_colors") <- c(
    attr(p, "series_colors"),
    stats::setNames(rep("black", nrow(ref)), ref$group)
  )

  p
}

# Reference lines as named black dashed lines in the linetype legend.
add_reference_lines <- function(p, reference_threshold) {
  if (is.null(reference_threshold) || length(reference_threshold) == 0) {
    return(p)
  }

  ref_labels <- paste0("Reference ", reference_threshold)

  for (i in seq_along(reference_threshold)) {
    p <- p +
      line_series_layer(
        ggplot2::geom_hline(
          yintercept = reference_threshold[[i]],
          color = "black",
          linetype = "dashed"
        ),
        ref_labels[[i]]
      )
  }

  return(p)
}

#' Add Horizontal References
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Adds horizontal reference lines to plot. Deprecated in favour of the
#' `reference_threshold` argument of the plotting functions. A plot styled with
#' [style_spec()] cannot be revealed or restyled after lines are added to it.
#'
#' @param p A ggplot object
#' @param reference_threshold Numeric/vector of numerics for horizontal lines
#'
#' @return A ggplot2 object with horizontal reference lines added
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' data_proc <- cqtkit_data_verapamil |> preprocess()
#' eda_mean_dv_over_time(
#'   data_proc,
#'   deltaQTCF,
#'   NTLD,
#'   DOSEF,
#'   group_col = TRTG,
#'   secondary_data_col = CONC,
#'   reference_dose = "0 mg"
#' ) |>
#'   add_horizontal_references(
#'     reference_threshold = c(-10, 10)
#'   )
add_horizontal_references <- function(p, reference_threshold) {
  lifecycle::deprecate_warn(
    when = "1.2.1",
    what = "add_horizontal_references()",
    details = "Use the `reference_threshold` argument of the plotting functions."
  )

  p <- add_reference_lines(p, reference_threshold)

  # The plot was styled before these lines existed, so set the linetypes of
  # all its named lines here, replacing the styled linetype scale.
  linetypes <- line_series_linetypes(p)
  if (length(linetypes) == 0) {
    return(p)
  }
  suppressMessages(
    p +
      ggplot2::scale_linetype_manual(
        values = linetypes,
        breaks = names(linetypes)
      )
  )
}


#' Add Secondary Data
#'
#' Adds secondary data to a plot.
#'
#' @param primary_data Dataframe of primary data plotted
#' @param secondary_data Dataframe containing data you'd like to add to plot
#' @param reference_threshold Reference threshold values
#' @param p A ggplot2 object to add data to
#' @param y_data String of column name in secondary data to use for plotting
#' @param group Grouping column
#' @param scale Multiplicative scaling factor
#' @param shift Additive shifting factor
#' @param sec_ylabel Secondary y axis label
#' @param ylabel Primary y axis label
#'
#' @importFrom rlang .data
#'
#' @return A ggplot2 object with secondary data layer and dual y-axis
add_secondary_data <- function(
  primary_data,
  secondary_data,
  reference_threshold = NULL,
  p,
  y_data,
  group,
  scale = NULL,
  shift = NULL,
  sec_ylabel,
  ylabel
) {
  group <- rlang::enquo(group)

  checkmate::assert_number(scale, null.ok = TRUE)
  checkmate::assert_number(shift, null.ok = TRUE)

  checkmate::assert(
    if (inherits(ylabel, "glue")) length(as.character(ylabel)) == 1 else TRUE,
    "ylabel must evaluate to a single value"
  )
  checkmate::assert(
    if (inherits(sec_ylabel, "glue")) length(as.character(sec_ylabel)) == 1 else
      TRUE,
    "sec_ylabel must evaluate to a single value"
  )

  # This is from https://finchstudio.io/blog/ggplot-dual-y-axes
  max_first <- max(primary_data[[y_data]])
  min_first <- min(primary_data[[y_data]])
  if (!is.null(reference_threshold)) {
    max_first <- max(max_first, max(c(reference_threshold)))
    min_first <- min(min_first, min(c(reference_threshold)))
  }

  max_second <- max(secondary_data[[y_data]])
  min_second <- min(secondary_data[[y_data]])

  # update args to have scale and shift if not provided
  if (is.null(scale)) {
    scale <- (max_second - min_second) / (max_first - min_first)
  }
  if (is.null(shift)) {
    shift <- min_first - min_second
  }

  p <- p +
    ggplot2::geom_line(
      data = secondary_data,
      ggplot2::aes(
        x = .data$time,
        y = inv_scale_function(.data[[y_data]], scale, shift),
        group = .data$grouping,
        color = .data$grouping,
      )
    ) +
    ggplot2::geom_point(
      data = secondary_data,
      ggplot2::aes(
        x = .data$time,
        y = inv_scale_function(.data[[y_data]], scale, shift),
        group = .data$grouping,
        color = .data$grouping,
        shape = .data$grouping
      )
    )

  secondary_groups <- unique(secondary_data$grouping)
  attr(p, "secondary_shapes") <- stats::setNames(
    rep(1, length(secondary_groups)),
    secondary_groups
  )

  p <- p +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        ~ scale_function(., scale, shift),
        name = sec_ylabel
      )
    )

  return(p)
}

# function to scale secondary axis
scale_function <- function(x, scale, shift) {
  return((x) * scale - shift)
}

# function to scale secondary variable values
inv_scale_function <- function(x, scale, shift) {
  return((x + shift) / scale)
}

update_ribbon_alpha <- function(p, alpha = NULL) {
  if (!is.null(alpha)) {
    for (i in seq_along(p$layers)) {
      if (inherits(p$layers[[i]]$geom, "GeomRibbon")) {
        p$layers[[i]]$aes_params$alpha <- alpha
      }
    }

    # Also override legend display for fill transparency
    p <- p +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(alpha = alpha))
      )
  }

  return(p)
}

get_color_groups <- function(p) {
  extract_groups(p, "colour")
}

get_fill_groups <- function(p) {
  extract_groups(p, "fill")
}

get_shape_groups <- function(p) {
  extract_groups(p, "shape")
}

get_linetype_groups <- function(p) {
  extract_groups(p, "linetype")
}

extract_groups <- function(p, aesthetic) {
  all_groups <- c()

  # global mappings
  if (!is.null(p$mapping[[aesthetic]])) {
    groups <- extract_from_mapping(p$mapping[[aesthetic]], p$data)
    all_groups <- c(all_groups, groups)
  }

  # layer mappings
  for (layer in p$layers) {
    data <- layer$data
    if (is.null(data) || inherits(data, "waiver")) data <- p$data

    if (!is.null(layer$mapping[[aesthetic]])) {
      groups <- extract_from_mapping(layer$mapping[[aesthetic]], data)
      all_groups <- c(all_groups, groups)
    }
  }

  unique(all_groups)
}

extract_from_mapping <- function(mapping_entry, data) {
  # A constant mapping, e.g. from ggstylekit::series_layer(), is one group.
  expr <- if (rlang::is_quosure(mapping_entry)) {
    rlang::quo_get_expr(mapping_entry)
  } else {
    mapping_entry
  }
  if (is.character(expr) && length(expr) == 1) {
    return(expr)
  }
  var <- rlang::as_label(mapping_entry)
  if (startsWith(var, ".data$")) {
    var <- sub("^\\.data\\$", "", var)
  }
  if (var %in% names(data)) {
    values <- data[[var]]
    # Factors carry their own order; `unique()` on the character values would
    # instead follow whatever order the rows happen to be in.
    if (is.factor(values)) {
      return(levels(droplevels(values)))
    }
    return(unique(as.character(values)))
  }
  return(NULL)
}


get_current_colors <- function(p) {
  built <- ggplot2::ggplot_build(p)

  color_mapping <- list()

  for (i in seq_along(built$data)) {
    layer_data <- built$data[[i]]
    if ("colour" %in% names(layer_data) && "group" %in% names(layer_data)) {
      unique_pairs <- unique(layer_data[, c("colour", "group")])

      if (!is.null(built$layout$panel_scales_y[[1]]$range$range)) {
        for (j in seq_len(nrow(unique_pairs))) {
          color_mapping[[as.character(unique_pairs$group[
            j
          ])]] <- unique_pairs$colour[j]
        }
      }
    }
  }

  return(color_mapping)
}

make_default_palette <- function(aesthetic, groups) {
  if (length(groups) == 0) return(character(0))

  switch(
    aesthetic,
    "shape" = {
      shape_val <- 16 # solid circle
      vals <- rep(shape_val, length(groups))
      names(vals) <- groups
      vals
    },
    "color" = {
      # Filter out reference groups to avoid affecting data group colors
      data_groups <- groups[!grepl("^Reference ", groups)]
      if (length(data_groups) > 0) {
        # Generate palette based only on data groups
        data_vals <- scales::hue_pal()(length(data_groups))
        names(data_vals) <- data_groups

        # Add any remaining groups (references) with default colors
        remaining_groups <- setdiff(groups, data_groups)
        if (length(remaining_groups) > 0) {
          remaining_vals <- rep("black", length(remaining_groups))
          names(remaining_vals) <- remaining_groups
          vals <- c(data_vals, remaining_vals)
        } else {
          vals <- data_vals
        }
      } else {
        vals <- scales::hue_pal()(length(groups))
        names(vals) <- groups
      }
      vals
    },
    "fill" = {
      vals <- scales::hue_pal()(length(groups)) # <- restore hue palette
      names(vals) <- groups
      vals
    },
    "linetype" = {
      linetype_val <- "solid" # default linetype
      vals <- rep(linetype_val, length(groups))
      names(vals) <- groups
      vals
    },
    character(0)
  )
}

apply_manual_scale <- function(
  p,
  aesthetic,
  groups,
  default_map = NULL,
  user_values = NULL,
  user_labels = NULL,
  scale_fn = NULL,
  alpha = NULL
) {
  # Skip if nothing to map
  if (length(groups) == 0 && is.null(user_values)) return(p)

  # Use correct zero-length vector type
  default_map <- default_map %||%
    switch(
      aesthetic,
      "shape" = integer(0),
      character(0)
    )

  # Build unified set of all groups to account for default/user inputs
  all_groups <- unique(c(
    groups,
    names(user_values %||% NULL),
    names(default_map)
  ))
  all_groups <- all_groups[!is.na(all_groups)]

  # Default palettes
  default_palette <- make_default_palette(aesthetic, groups)

  # Merge default, then user overrides
  final_map <- default_palette
  if (!is.null(default_map)) final_map[names(default_map)] <- default_map
  if (!is.null(user_values)) final_map[names(user_values)] <- user_values

  # Labels
  final_labels <- stats::setNames(all_groups, all_groups)
  if (!is.null(user_labels)) {
    overlapping <- intersect(names(user_labels), names(final_labels))
    final_labels[overlapping] <- user_labels[overlapping]
  }

  # Drop NA-labeled groups from legend but keep their colors in the scale
  valid_groups <- names(final_labels)[!is.na(final_labels)]
  final_labels <- final_labels[valid_groups]
  # Keep all colors in final_map, don't filter by valid_groups

  # Use master order from style_plot (colors → labels → shapes → remaining)
  # Only mapped groups get legend keys: a `colors` entry for a named line
  # (line_series_layer()) is applied to the layer, not the scale.
  master_order <- attr(p, "master_order")
  breaks <- intersect(master_order, intersect(names(final_labels), groups))
  labels <- final_labels[breaks]
  # Keep all values in final_map for aesthetic consistency (like colors)

  # Apply the scale
  if (!is.null(scale_fn)) {
    p <- p +
      suppressWarnings(scale_fn(
        values = final_map,
        breaks = breaks,
        labels = labels
      ))
  }

  # Optional alpha control
  if (!is.null(alpha) && aesthetic == "fill") {
    p <- update_ribbon_alpha(p, alpha)
  }

  return(p)
}
