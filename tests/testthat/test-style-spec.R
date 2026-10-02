test_that("combined spec plots remain restylable", {
  fit <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    method = "REML",
    remove_conc_iiv = TRUE
  )

  p <- gof_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_no_error(restyle_plot(p, legend.position = "bottom"))
  expect_no_error(reveal(p, time, as = "facet"))
})

legend_titles <- function(p) {
  guides <- ggplot2::ggplot_build(p)$plot$guides$params
  unname(vapply(guides, function(x) format(x$title %||% "NULL"), ""))
}

test_that("a colour legend_spec sets the treatment legend", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    style = style_spec(
      legends = legend_spec(channel = "color", title = "Treatment", order = 1)
    )
  )

  expect_equal(p$labels$colour, "Treatment")
  expect_equal(p$guides$guides$colour$params$order, 1)
  expect_true("Treatment" %in% legend_titles(p))
})

test_that("hiding the colour legend removes the treatment legend", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    style = style_spec(legends = legend_spec(channel = "color", hide = TRUE))
  )

  expect_false("Treatment Group" %in% legend_titles(p))
})

test_that("reveal() maps a covariate onto shapes", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    style = style_spec()
  ) |>
    reveal(SEX, as = "shapes")

  expect_true("SEX" %in% legend_titles(p))
  expect_length(unique(ggplot2::ggplot_build(p)$data[[1]]$shape), 2)
})

test_that("color palette functions also provide mapped fill defaults", {
  fit <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    method = "REML",
    remove_conc_iiv = TRUE
  )

  p <- predict_with_exposure_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    treatment_predictors = list(
      CONC = 0,
      deltaQTCFBL = 0,
      TRTG = "Verapamil HCL",
      TAFD = "0.5 HR"
    ),
    style = style_spec(colors = scales::hue_pal())
  )

  expect_no_error(ggplot2::ggplot_build(p))
})

panel_titles <- function(p) {
  vapply(
    seq_len(length(p$patches$plots) + 1),
    function(i) format(p[[i]]$labels$title %||% ""),
    character(1)
  )
}

test_that("eda_qtc_comparison_plot() puts a style_spec() title on the figure", {
  p <- eda_qtc_comparison_plot(
    cqtkit_data_verapamil,
    RR,
    QT,
    QTCB,
    QTCF,
    style = style_spec(title = "QTc comparison")
  )
  expect_equal(p$patches$annotation$title, "QTc comparison")
  expect_equal(panel_titles(p), c("QT", "QTCB", "QTCF"))
})
