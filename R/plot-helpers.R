#' adds errorbars to a plot
#'
#' @param data A dataframe from compute_grouped_mean_sd
#' @param p A ggplot object to add error bars to
#' @param reference_dose Reference dose value for comparison calculations
#' @param error_bars Type of errorbars to use (ci, se, sd, null)
#' @param conf_int Numeric confidence interval level (default: 0.9)
#'
#' @returns ggplot2 with errorbars
add_error_bars_to_plot <- function(
  data,
  p,
  reference_dose,
  error_bars,
  conf_int
) {
  # no reference dose error bars here
  if (is.null(reference_dose)) {
    if (!is.null(error_bars)) {
      if (error_bars == 'CI') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high)
          )
        caption <- paste0("errorbars represent ", round(conf_int * 100), "% CI")
      } else if (error_bars == 'SE') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_dv - .data$se,
              ymax = .data$mean_dv + .data$se
            )
          )
        caption <- paste0("errorbars represent SE")
      } else if (error_bars == 'SD') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_dv - .data$sd,
              ymax = .data$mean_dv + .data$sd,
              y = .data$mean_dv
            )
          )
        caption <- paste0("errorbars represent SD")
      }
    } else {
      caption <- paste0("")
    }
  } else {
    # reference dose error bars
    if (!is.null(error_bars)) {
      if (error_bars == 'CI') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(ymin = .data$ci_low_delta, ymax = .data$ci_up_delta)
          )
        caption <- paste0("errorbars represent ", round(conf_int * 100), "% CI")
      } else if (error_bars == 'SE') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_delta_dv - .data$delta_se,
              ymax = .data$mean_delta_dv + .data$delta_se
            )
          )
        caption <- paste0("errorbars represent SE")
      } else if (error_bars == 'SD') {
        p <- p +
          ggplot2::geom_errorbar(
            data = data,
            ggplot2::aes(
              ymin = .data$mean_delta_dv - .data$delta_sd,
              ymax = .data$mean_delta_dv + .data$delta_sd
            )
          )
        caption <- paste0("errorbars represent SD")
      }
    } else {
      caption <- paste0("")
    }
  }

  #should add label/annotation for thresholds if they aren't null...
  p <- p +
    ggplot2::labs(
      caption = paste(caption, "\n")
    )

  return(p)
}

#' adds horizontal reference lines to plot
#'
#' @param p A ggplot object
#' @param reference_threshold Numeric/vector of numerics for horizontal lines
#'
#' @returns a ggplot object with refence horizontal lines
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
  if (is.null(reference_threshold) || length(reference_threshold) == 0) {
    return(p)
  }

  ref_labels <- paste0("Reference ", reference_threshold)

  ref_data <- data.frame(
    yintercept = reference_threshold,
    group = ref_labels,
    stringsAsFactors = FALSE
  )

  p <- p +
    suppressWarnings(
      ggplot2::geom_hline(
        data = ref_data,
        ggplot2::aes(
          yintercept = .data$yintercept,
          color = .data$group,
          shape = .data$group # Needed for combined color/shape legend
        ),
        linetype = "dashed"
      )
    )

  attr(p, "reference_colors") <- stats::setNames(
    rep("black", length(reference_threshold)),
    ref_labels
  )

  # Also set reference shapes as NA so they don't appear in legend
  attr(p, "reference_shapes") <- stats::setNames(
    rep(NA, length(reference_threshold)),
    ref_labels
  )

  return(p)
}


#' adds secondary data to a plot
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
#' @returns a ggplot2 object with additional data
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
    scale = (max_second - min_second) / (max_first - min_first)
  }
  if (is.null(shift)) {
    shift = min_first - min_second
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
  var <- rlang::as_label(mapping_entry)
  if (startsWith(var, ".data$")) {
    var <- sub("^\\.data\\$", "", var)
  }
  if (var %in% names(data)) {
    return(unique(as.character(data[[var]])))
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
  master_order <- attr(p, "master_order")
  breaks <- intersect(master_order, names(final_labels))
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
