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
    ),
    line_linewidth = 1
  )
}

cqtkit_square_theme <- function() {
  ggplot2::theme_bw() + ggplot2::theme(aspect.ratio = 1)
}

cqtkit_style_plot <- function(p, style = ggstylekit::style_spec(), ...) {
  if (is.null(style)) style <- ggstylekit::style_spec()
  house <- ggstylekit::with_defaults(
    ggstylekit::style_spec(...),
    cqtkit_style_defaults()
  )
  ggstylekit::style_plot(p, ggstylekit::with_defaults(style, house))
}
