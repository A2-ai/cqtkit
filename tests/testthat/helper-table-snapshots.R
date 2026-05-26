snapshot_plot <- function(plot, name) {
  testthat::skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(name, plot)
}

# chromote (used by webshot2) can fail to launch headless Chrome on the
# first attempt but succeed on a retry, so wrap flaky browser calls.
with_retries <- function(fn, times = 5, delay = 1) {
  for (i in seq_len(times)) {
    res <- tryCatch(fn(), error = function(e) e)
    if (!inherits(res, "error")) {
      return(invisible(res))
    }
    if (i < times) Sys.sleep(delay)
  }
  stop(res)
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

  with_retries(function() {
    webshot2::webshot(
      url = html_path,
      file = png_path,
      selector = "table.gt_table",
      delay = 1,
      vwidth = 4000,
      vheight = 3000,
      zoom = 1
    )
  })

  testthat::expect_snapshot_file(png_path)
}
