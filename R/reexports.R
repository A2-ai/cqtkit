# Re-exports from ggstylekit so cqtkit users can build and adjust plot
# styles without attaching ggstylekit (which would mask the deprecated
# set_style()/style_plot() shims).

#' @importFrom ggstylekit style_spec
#' @export
ggstylekit::style_spec

#' @importFrom ggstylekit legend_spec
#' @export
ggstylekit::legend_spec

#' @importFrom ggstylekit reveal
#' @export
ggstylekit::reveal

#' @importFrom ggstylekit restyle_plot
#' @export
ggstylekit::restyle_plot
