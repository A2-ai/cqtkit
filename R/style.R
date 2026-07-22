#' Set Style
#'
#' Creates a style list for eda graphing functions.
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

#' Style Plot
#'
#' Styles a plot with provided colors and labels.
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
#' @param logx Logical, whether to use log scale for x-axis
#' @param logy Logical, whether to use log scale for y-axis
#' @param fill_alpha A numeric for controlling alpha of fill colors
#' @param fill_legend A string to replace fill legend title
#' @param fill_order A numeric for setting fill legend order
#' @param caption_hjust A string or numeric for caption horizontal justification ("left"/0, "center"/0.5, "right"/1)
#' @param legend_nrow A numeric for number of rows in legend
#'
#' @return A ggplot2 object with applied colors, labels, shapes, and theme settings
#' @export
#'
#' @examples
#' .p <- eda_mean_dv_over_time(
#'   cqtkit_data_verapamil,
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

  spec <- style_list_to_spec(
    list(
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
    ),
    colors_default = attr(p, "prediction_colors") %||% character(0),
    fill_default = attr(p, "fill_colors") %||% character(0),
    shapes_default = attr(p, "secondary_shapes") %||% integer(0)
  )

  ggstylekit::style_plot(p, spec)
}

style_list_to_spec <- function(
  style,
  colors_default = NULL,
  fill_default = NULL,
  shapes_default = NULL,
  linetypes_default = NULL
) {
  g <- function(key) style[[key]]
  merge_named <- function(defaults, overrides) {
    out <- defaults
    if (length(overrides) > 0) out[names(overrides)] <- overrides
    if (length(out) == 0) NULL else out
  }

  colors_map <- merge_named(colors_default, g("colors"))
  fill_map <- merge_named(fill_default, g("colors"))
  shapes_map <- merge_named(shapes_default, g("shapes"))
  linetypes_map <- merge_named(linetypes_default, NULL)

  legends <- list()
  add_legend <- function(channel, title = NULL, order = NULL, labels = NULL) {
    if (is.null(title) && is.null(order) && is.null(labels)) {
      return(invisible())
    }
    legends[[length(legends) + 1L]] <<- ggstylekit::legend_spec(
      channel = channel,
      title = title,
      order = order,
      labels = labels
    )
  }
  add_legend(
    "color",
    title = g("legend"),
    order = g("color_order"),
    labels = g("labels")
  )
  add_legend(
    "shape",
    title = g("shape_legend"),
    order = g("shape_order"),
    labels = g("labels")
  )
  add_legend(
    "fill",
    title = g("fill_legend"),
    order = g("fill_order"),
    labels = g("labels")
  )
  linetype_title <- if (
    !is.null(g("linetype_order")) || !is.null(g("legend_nrow"))
  ) {
    ""
  }
  add_legend(
    "linetype",
    title = linetype_title,
    order = g("linetype_order"),
    labels = g("labels")
  )

  ggstylekit::style_spec(
    colors = colors_map,
    fill = fill_map,
    shapes = shapes_map,
    linetypes = linetypes_map,
    title = g("title"),
    xlabel = g("xlabel"),
    ylabel = g("ylabel"),
    xlims = g("xlims"),
    ylims = g("ylims"),
    logx = g("logx"),
    logy = g("logy"),
    fill_alpha = g("fill_alpha"),
    legends = if (length(legends) > 0) legends else NULL,
    legend.position = g("legend.position"),
    legend.title.position = g("legend.title.position") %||% "top",
    legend.title.hjust = g("legend.title.hjust"),
    caption_hjust = g("caption_hjust"),
    legend_nrow = g("legend_nrow")
  )
}

cqtkit_style_defaults <- function() {
  ggstylekit::style_spec(
    theme = ggplot2::theme_bw(),
    legends = ggstylekit::legend_spec(channel = "color", title = "Treatment Group"),
    line_linewidth = 1
  )
}

cqtkit_square_theme <- function() {
  ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
}

cqtkit_style_plot <- function(p, style = list(), ...) {
  if (is.null(style)) style <- list()
  house <- ggstylekit::with_defaults(
    ggstylekit::style_spec(...),
    cqtkit_style_defaults()
  )
  user_spec <- style_list_to_spec(
    style,
    colors_default = attr(p, "prediction_colors") %||% character(0),
    fill_default = attr(p, "fill_colors") %||% character(0),
    shapes_default = attr(p, "secondary_shapes") %||% integer(0)
  )
  final <- ggstylekit::with_defaults(user_spec, house)
  ggstylekit::style_plot(p, final)
}
