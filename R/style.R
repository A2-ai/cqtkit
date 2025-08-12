#' Creates a style list for eda graphing functions
#'
#' @param style An optional named list of style arguments to update
#' @param title A string for a plot title
#' @param xlabel A string for x-axis label
#' @param ylabel A string for y-axis label
#' @param xlims A tuple of numbers specifying limits for x-axis
#' @param ylims A tuple of numbers specifying limits for y-axis
#' @param colors A named character vector for setting colors
#' @param labels A named character vector for setting legend labels
#' @param shapes A named character vector for setting geom_point shapes
#' @param legend A string for setting color legend title
#' @param shape_legend A string for setting shape legend title
#' @param color_order A numeric for setting color legend order
#' @param shape_order A numeric for setting shape legend order
#' @param linetype_order A numeric for setting linetype legend order
#' @param legend.position A string for legend position
#' @param logx A boolean for setting x-axis to log scale
#' @param logy A boolean for setting y-axis to log scale
#' @param fill_alpha A numeric for controlling alpha of fill colors
#' @param fill_legend A string to replace fill legend title
#' @param fill_order A numeric for setting fill legend order
#' @param legend.title.position A string for legend title position ("top", "left", "bottom", "right")
#' @param legend.title.hjust A string or numeric for legend title horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param caption_hjust A string or numeric for caption horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param legend_nrow A numeric for number of rows in legend
#'
#' @returns a named list for using with style_plot
#' @export
#'
#' @examples
#' data_proc <- cqtkit_data_verapamil |> preprocess()
#' style = set_style(
#'   colors = c(
#'     "0 mg Placebo" = "grey"
#'   ),
#'   labels = c(
#'     "Reference -10" = NA,
#'     "Reference 10" = "+/- 10 ms dQTcF",
#'     "0 mg Placebo" = "Placebo",
#'     "120 mg Verapamil" = "Verapamil"
#'   ),
#'   legend = "Treatment"
#' )
#' style
set_style <- function(
  style = NULL,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  xlims = NULL,
  ylims = NULL,
  colors = NULL,
  labels = NULL,
  shapes = NULL,
  legend = NULL,
  shape_legend = NULL,
  color_order = NULL,
  shape_order = NULL,
  linetype_order = NULL,
  legend.position = NULL,
  legend.title.position = NULL,
  legend.title.hjust = NULL,
  logx = NULL,
  logy = NULL,
  fill_alpha = NULL,
  fill_legend = NULL,
  fill_order = NULL,
  caption_hjust = NULL,
  legend_nrow = NULL
) {
  default_style <- list(
    title = NULL,
    xlabel = NULL,
    ylabel = NULL,
    xlims = NULL,
    ylims = NULL,
    colors = NULL,
    labels = NULL,
    shapes = NULL,
    legend = NULL,
    shape_legend = NULL,
    color_order = NULL,
    shape_order = NULL,
    linetype_order = NULL,
    legend.position = NULL,
    legend.title.position = "top",
    legend.title.hjust = NULL,
    logx = NULL,
    logy = NULL,
    fill_alpha = NULL,
    fill_legend = NULL,
    fill_order = NULL,
    caption_hjust = NULL,
    legend_nrow = NULL
  )

  if (!is.null(style)) {
    if (!is.list(style)) stop("style must be a list")
    unknown_keys <- setdiff(names(style), names(default_style))
    if (length(unknown_keys) > 0) {
      warning(paste(
        "Ignoring unknown keys in style list:",
        paste(unknown_keys, collapse = ", ")
      ))
    }
    default_style[names(style)] <- style
  }

  updates <- list(
    title = title,
    xlabel = xlabel,
    ylabel = ylabel,
    xlims = xlims,
    ylims = ylims,
    colors = colors,
    labels = labels,
    shapes = shapes,
    legend = legend,
    shape_legend = shape_legend,
    color_order = color_order,
    shape_order = shape_order,
    linetype_order = linetype_order,
    legend.position = legend.position,
    legend.title.position = legend.title.position,
    legend.title.hjust = legend.title.hjust,
    logx = logx,
    logy = logy,
    fill_alpha = fill_alpha,
    fill_legend = fill_legend,
    fill_order = fill_order,
    caption_hjust = caption_hjust,
    legend_nrow = legend_nrow
  )

  # Get the names of arguments that were explicitly provided (excluding 'style')
  call_args <- names(as.list(match.call())[-1])
  call_args <- setdiff(call_args, "style") # Remove 'style' argument

  for (name in names(updates)) {
    # Only update if the argument was explicitly provided in the function call
    if (name %in% call_args) {
      if (is.null(updates[[name]])) {
        # Explicitly set to NULL (don't let R remove the element)
        default_style[name] <- list(NULL)
      } else {
        default_style[[name]] <- updates[[name]]
      }
    }
  }

  s <- default_style # for brevity

  if (!is_plot_label(s$title)) {
    stop("title must be a character string, expression, or NULL")
  }
  if (!is_plot_label(s$xlabel)) {
    stop("xlabel must be a character string, expression, or NULL")
  }
  if (!is_plot_label(s$ylabel)) {
    stop("ylabel must be a character string, expression, or NULL")
  }
  if (!is_plot_label(s$legend)) {
    stop("legend must be a character string, expression, or NULL")
  }
  if (!is_plot_label(s$shape_legend)) {
    stop("shape_legend must be a character string, expression, or NULL")
  }

  if (!is.null(s$xlims)) {
    if (!is.numeric(s$xlims) || length(s$xlims) != 2) {
      stop("xlims must be a numeric vector of length 2")
    }
    if (s$xlims[1] >= s$xlims[2]) stop("xlims must be in increasing order")
  }

  if (!is.null(s$ylims)) {
    if (!is.numeric(s$ylims) || length(s$ylims) != 2) {
      stop("ylims must be a numeric vector of length 2")
    }
    if (s$ylims[1] >= s$ylims[2]) stop("ylims must be in increasing order")
  }

  if (!is.null(s$colors)) {
    if (
      !is.character(s$colors) ||
        is.null(names(s$colors)) ||
        any(names(s$colors) == "")
    ) {
      stop("colors must be a named character vector")
    }
  }

  if (!is.null(s$labels)) {
    if (
      !is.vector(s$labels) ||
        is.null(names(s$labels)) ||
        any(names(s$labels) == "")
    ) {
      stop("labels must be a named vector")
    }
  }

  if (!is.null(s$shapes)) {
    if (
      !is.vector(s$shapes) ||
        is.null(names(s$shapes)) ||
        any(names(s$shapes) == "")
    ) {
      stop("labels must be a named vector")
    }
  }

  valid_positions <- c("left", "right", "bottom", "top", "none")
  if (
    !is.null(s$legend.position) && !(s$legend.position %in% valid_positions)
  ) {
    stop(paste(
      "legend.position must be one of:",
      paste(valid_positions, collapse = ", ")
    ))
  }

  valid_title_positions <- c("left", "right", "bottom", "top")
  if (
    !is.null(s$legend.title.position) &&
      !(s$legend.title.position %in% valid_title_positions)
  ) {
    stop(paste(
      "legend.title.position must be one of:",
      paste(valid_title_positions, collapse = ", ")
    ))
  }

  if (!is.null(s$logx) && !is.logical(s$logx)) {
    stop("logx must be a logical or NULL")
  }
  if (!is.null(s$logy) && !is.logical(s$logy)) {
    stop("logy must be a logical or NULL")
  }

  if (!is.null(s$logx) && s$logx && !is.null(s$xlims) && any(s$xlims <= 0)) {
    warning("xlims should be > 0 when logx is TRUE")
  }

  if (!is.null(s$logy) && s$logy && !is.null(s$ylims) && any(s$ylims <= 0)) {
    warning("ylims should be > 0 when logy is TRUE")
  }

  if (!is.null(s$fill_alpha)) {
    if (s$fill_alpha > 1 || s$fill_alpha < 0) {
      stop("fill_alpha must be between 0 and 1")
    }
  }

  if (!is.null(s$legend_nrow)) {
    if (
      !is.numeric(s$legend_nrow) ||
        s$legend_nrow < 1 ||
        s$legend_nrow != round(s$legend_nrow)
    ) {
      stop("legend_nrow must be a positive integer")
    }
  }

  return(s)
}

# helper function for checking labels
is_plot_label <- function(x) {
  is.null(x) || is.character(x) || is.language(x) || inherits(x, "expression")
}

#' Styles a plot with provided colors and labels
#'
#' @param p A ggplot2 object to update colors/legend labels
#' @param title A string for a plot title
#' @param xlabel A string for x-axis label
#' @param ylabel A string for y-axis label
#' @param xlims A tuple of numbers specifying limits for x-axis
#' @param ylims A tuple of numbers specifying limits for y-axis
#' @param colors A named character vector for setting colors
#' @param labels A named character vector for setting legend labels
#' @param shapes A named character vector for setting geom_point shapes
#' @param legend A string for setting color legend title
#' @param shape_legend A string for setting shape legend title
#' @param color_order A numeric for setting color legend order
#' @param shape_order A numeric for setting shape legend order
#' @param linetype_order A numeric for setting linetype legend order
#' @param legend.position A string for legend position
#' @param legend.title.position A string for legend title position ("top", "left", "bottom", "right")
#' @param legend.title.hjust A string or numeric for legend title horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param logx A boolean for setting x-axis to log scale
#' @param logy A boolean for setting y-axis to log scale
#' @param fill_alpha A numeric for controlling alpha of fill colors
#' @param fill_legend A string to replace fill legend title
#' @param fill_order A numeric for setting fill legend order
#' @param caption_hjust A string or numeric for caption horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param legend_nrow A numeric for number of rows in legend
#'
#' @returns an updated ggplot2 object
#' @export
#'
#' @examples
#' data_proc <- cqtkit_data_verapamil |> preprocess()
#' .p <- eda_mean_dv_over_time(
#'   data_proc,
#'   deltaQTCF,
#'   NTLD,
#'   DOSEF,
#'   group_col = TRTG,
#'   reference_threshold = c(-10, 10),
#' )
#'
#' .p <- style_plot(
#'   p = .p,
#'   colors = c(
#'     "0 mg Placebo" = "grey"
#'   ),
#'   labels = c(
#'     "120 mg Verapamil HCL" = "Verapamil",
#'     "0 mg Placebo" = "Placebo",
#'     "Reference 10" = "+/- 10 ms dQTcF",
#'     "Reference -10" = NA
#'   ),
#'   legend = "Treatment"
#' )
#' .p
style_plot <- function(
  p,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  xlims = NULL,
  ylims = NULL,
  colors = NULL,
  labels = NULL,
  shapes = NULL,
  legend = NULL,
  shape_legend = NULL,
  color_order = NULL,
  shape_order = NULL,
  linetype_order = NULL,
  legend.position = NULL,
  legend.title.position = NULL,
  legend.title.hjust = NULL,
  logx = NULL,
  logy = NULL,
  fill_alpha = NULL,
  fill_legend = NULL,
  fill_order = NULL,
  caption_hjust = NULL,
  legend_nrow = NULL
) {
  if (!inherits(p, "ggplot")) {
    stop("Must provide a ggplot object")
  }

  # Unified group collection
  color_groups <- get_color_groups(p)
  shape_groups <- get_shape_groups(p)
  fill_groups <- get_fill_groups(p)
  linetype_groups <- get_linetype_groups(p)

  all_groups <- unique(c(
    color_groups,
    shape_groups,
    fill_groups,
    linetype_groups
  ))

  # Create master order: user-supplied values first, then all detected
  master_order <- c()
  if (!is.null(labels)) master_order <- c(master_order, names(labels))
  if (!is.null(colors)) master_order <- c(master_order, names(colors))
  if (!is.null(shapes)) master_order <- c(master_order, names(shapes))

  remaining_groups <- setdiff(all_groups, master_order)
  master_order <- unique(c(master_order, remaining_groups))

  attr(p, "master_order") <- master_order

  # Color scale
  p <- apply_manual_scale(
    p,
    aesthetic = "color",
    groups = color_groups,
    default_map = c(
      attr(p, "reference_colors") %||% character(0),
      attr(p, "prediction_colors") %||% character(0)
    ),
    user_values = colors,
    user_labels = labels,
    scale_fn = ggplot2::scale_color_manual
  )

  # Fill scale
  if (length(fill_groups) > 0) {
    p <- apply_manual_scale(
      p,
      aesthetic = "fill",
      groups = fill_groups,
      default_map = attr(p, "fill_colors") %||% character(0),
      user_values = colors,
      user_labels = labels,
      scale_fn = ggplot2::scale_fill_manual,
      alpha = fill_alpha
    )
  }

  # Shape scale
  full_shape_groups <- unique(c(shape_groups, color_groups))
  if (length(full_shape_groups) > 0) {
    expanded_shapes <- shapes %||% integer(0)

    # Fill in missing shape values
    missing_groups <- setdiff(full_shape_groups, names(expanded_shapes))
    if (length(missing_groups) > 0) {
      secondary_groups <- names(attr(p, "secondary_shapes") %||% integer(0))

      missing_shapes <- integer(0)
      for (group in missing_groups) {
        missing_shapes[group] <- if (group %in% secondary_groups) 1 else 16
      }

      expanded_shapes <- c(expanded_shapes, missing_shapes)
    }

    # Store final shape map if needed elsewhere
    attr(p, "final_shape_map") <- expanded_shapes

    p <- apply_manual_scale(
      p,
      aesthetic = "shape",
      groups = full_shape_groups,
      default_map = integer(0),
      user_values = expanded_shapes,
      user_labels = labels,
      scale_fn = ggplot2::scale_shape_manual
    )
  }

  # Linetype scale
  if (length(linetype_groups) > 0) {
    p <- apply_manual_scale(
      p,
      aesthetic = "linetype",
      groups = linetype_groups,
      default_map = attr(p, "linetype_values") %||% character(0),
      user_values = NULL,
      user_labels = labels,
      scale_fn = ggplot2::scale_linetype_manual
    )
  }

  # Axis labels and legend titles
  if (!is.null(legend)) {
    p <- p + ggplot2::labs(color = legend)

    if (
      length(full_shape_groups) > 0 && all(full_shape_groups %in% color_groups)
    ) {
      p <- p + ggplot2::labs(shape = legend)
    }
  }

  if (!is.null(fill_legend)) p <- p + ggplot2::labs(fill = fill_legend)
  if (!is.null(legend.position)) {
    p <- p + ggplot2::theme(legend.position = legend.position)
  }
  if (!is.null(legend.title.position)) {
    p <- p + ggplot2::theme(legend.title.position = legend.title.position)
  }
  if (!is.null(legend.title.hjust)) {
    if (is.character(legend.title.hjust)) {
      hjust_value <- switch(
        legend.title.hjust,
        "left" = 0,
        "center" = 0.5,
        "right" = 1,
        stop(
          "legend.title.hjust must be 'left', 'center', 'right', or a numeric value between 0 and 1"
        )
      )
    } else if (is.numeric(legend.title.hjust)) {
      hjust_value <- legend.title.hjust
    } else {
      stop(
        "legend.title.hjust must be 'left', 'center', 'right', or a numeric value between 0 and 1"
      )
    }
    p <- p +
      ggplot2::theme(legend.title = ggplot2::element_text(hjust = hjust_value))
  }

  # Legend ordering
  guide_list <- list()
  if (!is.null(color_order) || !is.null(legend_nrow)) {
    guide_args <- list()
    if (!is.null(color_order)) guide_args$order <- color_order
    if (!is.null(legend_nrow)) guide_args$nrow <- legend_nrow
    guide_list$color <- do.call(ggplot2::guide_legend, guide_args)
  }
  if (!is.null(shape_order) || !is.null(legend_nrow)) {
    guide_args <- list()
    if (!is.null(shape_order)) guide_args$order <- shape_order
    if (!is.null(legend_nrow)) guide_args$nrow <- legend_nrow
    guide_list$shape <- do.call(ggplot2::guide_legend, guide_args)
  }
  if (!is.null(fill_order) || !is.null(legend_nrow)) {
    guide_args <- list()
    if (!is.null(fill_order)) guide_args$order <- fill_order
    if (!is.null(legend_nrow)) guide_args$nrow <- legend_nrow
    guide_list$fill <- do.call(ggplot2::guide_legend, guide_args)
  }
  if (!is.null(linetype_order) || !is.null(legend_nrow)) {
    guide_args <- list(title = "")
    if (!is.null(linetype_order)) guide_args$order <- linetype_order
    if (!is.null(legend_nrow)) guide_args$nrow <- legend_nrow
    guide_list$linetype <- do.call(ggplot2::guide_legend, guide_args)
  }

  if (length(guide_list) > 0) {
    p <- p + do.call(ggplot2::guides, guide_list)
  }

  # Axes and scales
  if (!is.null(xlims) || !is.null(ylims)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlims, ylim = ylims)
  }
  if (!is.null(xlabel)) p <- p + ggplot2::labs(x = xlabel)
  if (!is.null(ylabel)) p <- p + ggplot2::labs(y = ylabel)
  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  if (!is.null(logx) && logx) p <- p + ggplot2::scale_x_log10()
  if (!is.null(logy) && logy) p <- p + ggplot2::scale_y_log10()
  if (!is.null(caption_hjust)) {
    if (is.character(caption_hjust)) {
      hjust_value <- switch(
        caption_hjust,
        "left" = 0,
        "center" = 0.5,
        "right" = 1,
        stop(
          "caption_hjust must be 'left', 'center', 'right', or a numeric value between 0 and 1"
        )
      )
    } else if (is.numeric(caption_hjust)) {
      hjust_value <- caption_hjust
    } else {
      stop(
        "caption_hjust must be 'left', 'center', 'right', or a numeric value between 0 and 1"
      )
    }
    p <- p +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = hjust_value)
      )
  }

  return(p)
}
