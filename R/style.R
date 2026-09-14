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

# ---- Removed pre-2.0.0 styling API ----------------------------------------

#' Set Style (removed)
#'
#' Removed in cqtkit 2.0.0. Plots are styled with \pkg{ggstylekit}: build a
#' \code{style_spec()} and pass it to a plotting function's \code{style}
#' argument. See \code{vignette("styling", package = "cqtkit")}.
#'
#' @param ... Ignored.
#'
#' @return Nothing; this function always errors.
#' @seealso \code{\link[ggstylekit]{style_spec}},
#'   \code{\link[ggstylekit]{legend_spec}}
#' @export
set_style <- function(...) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "set_style()",
    with = "style_spec()",
    details = c(
      "Build the style with `style_spec()` and `legend_spec()`.",
      'See `vignette("styling", package = "cqtkit")`.'
    )
  )
}

#' Style Plot (removed)
#'
#' Removed in cqtkit 2.0.0. Pass a \code{style_spec()} to the plotting
#' function's \code{style} argument, or update a finished plot with
#' \code{restyle_plot()}. See \code{vignette("styling", package = "cqtkit")}.
#'
#' @param ... Ignored.
#'
#' @return Nothing; this function always errors.
#' @seealso \code{\link[ggstylekit]{restyle_plot}},
#'   \code{\link[ggstylekit]{style_spec}}
#' @export
style_plot <- function(...) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "style_plot()",
    with = "restyle_plot()",
    details = c(
      "Pass a `style_spec()` to the plotting function's `style` argument,",
      "or update a finished plot with `restyle_plot()`.",
      'See `vignette("styling", package = "cqtkit")`.'
    )
  )
}
