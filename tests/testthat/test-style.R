mean_dv_plot <- function(...) {
  eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    reference_threshold = c(-10, 10),
    ...
  )
}

test_that("set_style() and style_plot() are defunct with a pointer", {
  expect_error(set_style(title = "T"), "style_spec")
  expect_error(set_style(ggstylekit::style_spec(), xlabel = "x"), "style_spec")
  expect_error(style_plot(mean_dv_plot(), title = "T"), "restyle_plot")
})

test_that("a plain list passed to style is an error", {
  expect_error(
    eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, style = list(title = "Plain")),
    "style_spec", fixed = TRUE
  )
  expect_error(
    eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, style = "nope"),
    "style_spec", fixed = TRUE
  )
})

test_that("ggstylekit constructors are re-exported", {
  expect_identical(style_spec, ggstylekit::style_spec)
  expect_identical(legend_spec, ggstylekit::legend_spec)
  expect_identical(reveal, ggstylekit::reveal)
  expect_identical(restyle_plot, ggstylekit::restyle_plot)
})
