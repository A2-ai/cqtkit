snapshot_plot <- function(plot, name, width = 8, height = 6) {
  png_path <- file.path(tempdir(), paste0(name, ".png"))
  ggplot2::ggsave(png_path, plot, width = width, height = height, dpi = 150)
  testthat::expect_snapshot_file(png_path)
}

snapshot_gt <- function(table, name) {
  testthat::skip_if_not_installed("gt")

  html_path <- file.path(tempdir(), paste0(name, ".html"))
  html_snapshot_path <- file.path(tempdir(), paste0(name, "-snapshot.html"))
  html <- gt::as_raw_html(table)
  # gt generates a random 10-char div ID on each call — normalize it
  html <- gsub("<div id=\"[A-Za-z0-9]{10}\"", "<div id=\"gt-table\"", html)
  writeLines(html, html_snapshot_path)
  testthat::expect_snapshot_file(html_snapshot_path)

  testthat::skip_on_ci()
  testthat::skip_on_os("windows")
  testthat::skip_on_os("linux")
  testthat::skip_if_not_installed("webshot2")
  png_path <- file.path(tempdir(), paste0(name, ".png"))
  gt::gtsave(table, filename = html_path)

  webshot2::webshot(
    url = html_path,
    file = png_path,
    selector = "table.gt_table",
    delay = 1,
    vwidth = 4000,
    vheight = 3000,
    zoom = 1
  )

  testthat::expect_snapshot_file(png_path)
}
