#' Set Style
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Creates a style list for eda graphing functions. Deprecated in favour of
#' [style_spec()].
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
#' @param logx Logical, whether to use log scale for x-axis
#' @param logy Logical, whether to use log scale for y-axis
#' @param fill_alpha A numeric for controlling alpha of fill colors
#' @param fill_legend A string to replace fill legend title
#' @param fill_order A numeric for setting fill legend order
#' @param legend.title.position A string for legend title position ("top", "left", "bottom", "right")
#' @param legend.title.hjust A string or numeric for legend title horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param caption_hjust A string or numeric for caption horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param legend_nrow A numeric for number of rows in legend
#'
#' @return A named list of style parameters for use with style_plot()
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
  lifecycle::deprecate_warn(
    when = "1.2.0",
    what = "set_style()",
    with = "style_spec()"
  )

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

#' Style Plot
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Styles a plot with provided colors and labels. Deprecated in favour of
#' [style_spec()] and [restyle_plot()].
#'
#' @param p A ggplot2 object to update colors/legend labels
#' @param ... Style arguments, documented in [set_style()]
#'
#' @return A ggplot2 object with applied colors, labels, shapes, and theme settings
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
  ...
) {
  lifecycle::deprecate_warn(
    when = "1.2.0",
    what = "style_plot()",
    with = "restyle_plot()"
  )
  style_plot_impl(p, ...)
}

# The list styling engine. cqtkit calls this directly so that a default
# `style = list()` call does not warn; only style_plot() itself is deprecated.
style_plot_impl <- function(
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

# ---------------------------------------------------------------------------
# ggstylekit path
#
# cqtkit carries its own house defaults as a style_spec() and resolves the
# caller's spec against them field by field, so anything the caller leaves NULL
# falls through to cqtkit's default. The list engine above is untouched.
# ---------------------------------------------------------------------------

# What cqtkit hardcodes today, written as a spec.
cqtkit_style_defaults <- function() {
  ggstylekit::style_spec(
    theme = ggplot2::theme_bw(),
    legends = list(
      ggstylekit::legend_spec(
        channel = "color",
        title = "Treatment Group",
        order = 1
      ),
      ggstylekit::legend_spec(channel = "linetype", title = "")
    )
  )
}

# Resolve caller spec against per-function defaults, then the house defaults.
cqtkit_style_plot <- function(p, style, ...) {
  caller_legends <- style$legends
  style <- ggstylekit::with_defaults(style, ggstylekit::style_spec(...))
  style <- ggstylekit::with_defaults(style, cqtkit_style_defaults())
  style <- unify_shape_legend(style, p)
  if (!is.null(caller_legends)) {
    style <- ggstylekit::with_defaults(
      ggstylekit::style_spec(legends = caller_legends),
      style
    )
  }
  style <- default_fill_from_colors(style, p)
  ggstylekit::style_plot(p, style)
}

# The list engine gives the shape guide the colour legend's title, so the two
# render as one guide. Mirror that here, and do it after the caller's spec has
# been resolved: a caller who renames the colour legend has to move both, or
# ggplot2 sees two titles and draws two legends.
unify_shape_legend <- function(style, p) {
  shape_groups <- unique(c(get_shape_groups(p), get_color_groups(p)))
  color_groups <- get_color_groups(p)
  if (length(shape_groups) == 0 || !all(shape_groups %in% color_groups)) {
    return(style)
  }

  channel_of <- function(entry) entry$channel %||% ""
  color_entry <- Find(function(e) channel_of(e) == "colors", style$legends)
  if (is.null(color_entry)) {
    return(style)
  }

  style$legends <- lapply(style$legends, function(entry) {
    if (channel_of(entry) != "shapes") {
      return(entry)
    }
    entry$title <- color_entry$title
    entry$labels <- color_entry$labels
    entry$order <- color_entry$order
    entry
  })

  style
}

# The list engine passes one `colors` map to both the colour and the fill
# scale. style_spec() keeps them apart, so mirror it: fill defaults to the
# resolved colours laid over whatever the plot stashed as fill_colors. Run
# after the merge so a caller's own colours reach the ribbons too.
default_fill_from_colors <- function(style, p) {
  if (!is.null(style$fill) || length(get_fill_groups(p)) == 0) {
    return(style)
  }

  if (is.function(style$colors)) {
    fill_defaults <- attr(p, "fill_colors")
    if (length(fill_defaults) == 0L) {
      fill_defaults <- NULL
    }
    resolved <- ggstylekit::with_defaults(
      ggstylekit::style_spec(fill = style$colors),
      ggstylekit::style_spec(fill = fill_defaults)
    )
    style$fill <- resolved$fill
    return(style)
  }

  # Build it the way the list engine does: fill_colors is the base, the
  # resolved colours override by name and append new names. The order is load
  # bearing, ggplot2 falls back to positional assignment here.
  fill <- attr(p, "fill_colors") %||% character(0)
  if (length(style$colors) > 0) {
    fill[names(style$colors)] <- style$colors
  }
  if (length(fill) > 0) {
    style$fill <- fill
  }
  style
}

# Dispatch rather than gate: NULL and a bare list keep the list engine, a
# ggstylekit_style_spec takes the spec path.
as_style_spec <- function(style) {
  if (is.null(style)) {
    return(list())
  }
  if (inherits(style, "ggstylekit_style_spec")) {
    return(style)
  }
  if (!is.list(style)) {
    stop("style must be a list or a ggstylekit style_spec()")
  }
  style
}

is_style_spec <- function(style) {
  inherits(style, "ggstylekit_style_spec")
}

# Compose styled panels without baking the spec path into grobs. The legacy
# path stays on ggpubr so existing list-styled and default figures retain their
# current rendering. A collected patchwork legend takes its initial position
# from the public function argument; later restyle_plot() calls can move it.
compose_cqtkit_plots <- function(
  plots,
  style,
  nrow = NULL,
  ncol = NULL,
  legend_location = "top",
  common_legend = TRUE,
  title = NULL
) {
  legend_position <- if (common_legend) legend_location else "none"

  if (is_style_spec(style)) {
    plots <- unname(plots)
    combined <- do.call(
      ggstylekit::combine_styled_plots,
      c(plots, list(nrow = nrow, ncol = ncol))
    )
    combined <- ggstylekit::restyle_plot(
      combined,
      legend.position = legend_position
    )
    if (!is.null(title)) {
      combined <- combined + patchwork::plot_annotation(title = title)
    }
    return(combined)
  }

  combined <- ggpubr::ggarrange(
    plotlist = plots,
    nrow = nrow,
    ncol = ncol,
    common.legend = common_legend,
    legend = legend_position
  )
  if (!is.null(title)) {
    combined <- ggpubr::annotate_figure(combined, top = title)
  }
  combined
}

# A GOF style title belongs to the assembled figure. Remove it from spec-styled
# panels while preserving the class and explicit NULL field expected by
# ggstylekit's default resolution.
without_panel_title <- function(style) {
  if (is_style_spec(style)) {
    style["title"] <- list(NULL)
  }
  style
}

# The scale values the list engine reads off the plot object. The `linetype_values`
# attribute is not among them: get_linetype_groups() returns nothing, so
# style_plot() skips the linetype branch and ggplot2's default discrete palette
# is what draws LOESS solid and Linear dashed today.
plot_scale_defaults <- function(p) {
  # The list engine's colour scale reads these two and not fill_colors, which
  # belongs to the fill scale alone.
  colors <- c(
    attr(p, "reference_colors") %||% character(0),
    attr(p, "prediction_colors") %||% character(0)
  )
  list(
    colors = dedupe_by_name(colors),
    shapes = default_shape_map(p)
  )
}

# The list engine gives every colour or shape group an explicit shape, 16 for a
# primary group and 1 for a secondary one. Without the same map the spec path
# falls back to ggplot2's default shape 19, which draws a stroked point where
# the list path draws a solid one.
default_shape_map <- function(p) {
  groups <- unique(c(get_shape_groups(p), get_color_groups(p)))
  if (length(groups) == 0) {
    return(NULL)
  }
  secondary <- names(attr(p, "secondary_shapes") %||% integer(0))
  shapes <- stats::setNames(
    ifelse(groups %in% secondary, 1L, 16L),
    groups
  )
  dedupe_by_name(shapes)
}

# style_spec() rejects a scale map with repeated names. The plot attributes and
# the per-function defaults can name the same group, so the first value wins.
dedupe_by_name <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x[!duplicated(names(x))]
}

# Theme for the square gof_* panels, which set aspect.ratio = 1 at construction.
cqtkit_square_theme <- function() {
  ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
}

# The order style_plot() builds for its scale breaks: caller-named groups
# first, then everything else in detection order. legend_spec(levels = ) is
# how the spec path asks for the same sequence.
master_order <- function(p, labels, colors, shapes) {
  named <- unique(c(names(labels), names(colors), names(shapes)))
  detected <- unique(c(
    get_color_groups(p),
    get_shape_groups(p),
    get_fill_groups(p),
    get_linetype_groups(p)
  ))
  unique(c(named, setdiff(detected, named)))
}

# Single translation point between the two vocabularies. Per-function defaults
# arrive in the legacy names; on the spec path the legend ones become
# legend_spec() entries and everything else carries across unchanged.
cqtkit_apply_style <- function(
  p,
  style,
  title = NULL,
  xlabel = NULL,
  ylabel = NULL,
  xlims = NULL,
  ylims = NULL,
  colors = NULL,
  fill_alpha = NULL,
  legend = NULL,
  shape_legend = NULL,
  fill_legend = NULL,
  labels = NULL,
  color_order = NULL,
  shape_order = NULL,
  linetype_order = NULL,
  fill_order = NULL,
  theme = NULL
) {
  style$title <- style$title %||% title
  style$xlabel <- style$xlabel %||% xlabel
  style$ylabel <- style$ylabel %||% ylabel
  style$xlims <- style$xlims %||% xlims
  style$ylims <- style$ylims %||% ylims
  style$fill_alpha <- style$fill_alpha %||% fill_alpha

  if (!is_style_spec(style)) {
    style$colors <- style$colors %||% colors
    style$legend <- style$legend %||% legend
    style$shape_legend <- style$shape_legend %||% shape_legend
    style$fill_legend <- style$fill_legend %||% fill_legend
    style$labels <- style$labels %||% labels
    style$color_order <- style$color_order %||% color_order
    style$shape_order <- style$shape_order %||% shape_order
    style$linetype_order <- style$linetype_order %||% linetype_order
    style$fill_order <- style$fill_order %||% fill_order
    return(do.call(style_plot_impl, c(list(p = p), style)))
  }

  scales <- plot_scale_defaults(p)
  levels <- master_order(p, labels, colors, scales$shapes)

  legends <- list()
  if (!is.null(legend) || !is.null(labels) || !is.null(color_order)) {
    legends <- c(
      legends,
      list(ggstylekit::legend_spec(
        channel = "color",
        title = legend,
        labels = labels,
        order = color_order,
        levels = levels
      ))
    )
  }
  shape_title <- shape_legend %||% legend
  if (!is.null(shape_title) || !is.null(labels) || !is.null(shape_order)) {
    legends <- c(
      legends,
      list(ggstylekit::legend_spec(
        channel = "shape",
        title = shape_title,
        labels = labels,
        levels = levels,
        # ggplot2 merges the colour and shape guides only when their order
        # matches too, and the house colour legend sits at 1.
        order = shape_order %||% color_order %||% 1
      ))
    )
  }
  if (!is.null(fill_legend) || !is.null(fill_order)) {
    legends <- c(
      legends,
      list(ggstylekit::legend_spec(
        channel = "fill",
        title = fill_legend,
        order = fill_order,
        levels = levels
      ))
    )
  }
  if (!is.null(linetype_order)) {
    legends <- c(
      legends,
      list(ggstylekit::legend_spec(
        channel = "linetype",
        order = linetype_order
      ))
    )
  }

  default_colors <- dedupe_by_name(c(colors, scales$colors))

  cqtkit_style_plot(
    p,
    style,
    caption = p$labels$caption,
    colors = default_colors,
    shapes = scales$shapes,
    legends = if (length(legends) > 0) legends else NULL,
    theme = theme
  )
}
