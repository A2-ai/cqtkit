test_that("eda_qt_rr_plot default snapshot", {

  p <- eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lm")

  snapshot_plot(p, "eda-qt-rr-default")
})

test_that("eda_qt_rr_plot with slope p-value snapshot", {
  p <- eda_qt_rr_plot(
    cqtkit_data_verapamil, RR, QT, ID,
    model_type = "lm",
    show_model_results = model_results_spec(pvalue = TRUE)
  )

  snapshot_plot(p, "eda-qt-rr-pvalue")
})

test_that("eda_qt_rr_plot with style snapshot", {

  p <- eda_qt_rr_plot(
    cqtkit_data_verapamil, RR, QT, ID, TRTG,
    model_type = "lme",
    style = ggstylekit::style_spec(
      title = "QT-RR Relationship",
      xlabel = "RR Interval (ms)",
      ylabel = "QT Interval (ms)",
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      legends = ggstylekit::legend_spec(channel = "color", title = "Treatment"),
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "eda-qt-rr-styled")
})

test_that("eda_qtc_comparison_plot snapshot", {

  p <- eda_qtc_comparison_plot(
    cqtkit_data_verapamil, RR, QT, QTCB, QTCF,
    id_col = ID, trt_col = TRTG,
    model_type = "lme",
    show_model_results = TRUE,
    remove_rr_iiv = TRUE
  )

  snapshot_plot(p, "eda-qtc-comparison")
})

test_that("eda_quantiles_plot default snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0)

  p <- eda_quantiles_plot(data_proc, CONC, deltaQTCF, trt_col = TRTG)

  snapshot_plot(p, "eda-quantiles-default")
})

test_that("eda_quantiles_plot with style snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0)

  p <- eda_quantiles_plot(
    data_proc, CONC, deltaQTCF, trt_col = TRTG,
    plot_observations = TRUE,
    style = ggstylekit::style_spec(
      xlabel = "Concentration (ng/mL)",
      ylabel = "deltaQTcF (ms)",
      ylims = c(-40, 40),
      legends = ggstylekit::legend_spec(channel = "color", title = "Treatment"),
      legend.position = "top"
    )
  )

  snapshot_plot(p, "eda-quantiles-styled")
})

test_that("eda_scatter_with_regressions default snapshot", {

  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil, deltaQTCF, CONC, TRTG,
    reference_threshold = 10
  )

  snapshot_plot(p, "eda-scatter-regressions-default")
})

test_that("eda_mean_dv_over_time default snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    reference_threshold = 10
  )

  snapshot_plot(p, "eda-mean-dv-time-default")
})

test_that("eda_mean_dv_over_time with reference dose and style snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    reference_dose = "0 mg",
    reference_threshold = c(-10, 10),
    style = ggstylekit::style_spec(
      ylabel = bquote("Mean " ~ Delta ~ Delta ~ "QTcF (ms)"),
      xlabel = "Time (hr)",
      legends = ggstylekit::legend_spec(channel = "color", title = "Treatment"),
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "eda-mean-dv-time-styled")
})

test_that("eda_mean_dv_over_time dQTcF with PK overlay snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    secondary_data_col = CONC,
    group_col = TRTG,
    reference_threshold = 10
  )

  snapshot_plot(p, "eda-mean-dv-time-pk-dqtcf")
})

test_that("eda_mean_dv_over_time ddQTcF with PK overlay snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    secondary_data_col = CONC,
    group_col = TRTG,
    reference_dose = "0 mg",
    reference_threshold = c(-10, 10)
  )

  snapshot_plot(p, "eda-mean-dv-time-pk-ddqtcf")
})

test_that("eda_scatter_with_regressions styled snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0)

  p <- eda_scatter_with_regressions(
    data_proc, deltaQTCF, CONC, TRTG,
    reference_threshold = c(-10, 10),
    style = ggstylekit::style_spec(
      title = "Concentration-QTc Relationship",
      xlabel = "Concentration (ng/mL)",
      ylabel = "deltaQTcF (ms)",
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      legend_nrow = 1,
      legend.title.hjust = "center",
      caption_hjust = "right",
      legends = list(
        ggstylekit::legend_spec(channel = "color", order = 1),
        ggstylekit::legend_spec(channel = "linetype", order = 2)
      )
    )
  )

  snapshot_plot(p, "eda-scatter-styled")
})

test_that("eda_quantiles_plot with log axes snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0)

  p <- eda_quantiles_plot(
    data_proc, CONC, QTCF, trt_col = TRTG,
    style = ggstylekit::style_spec(
      logx = TRUE,
      logy = TRUE,
      xlabel = "Concentration (ng/mL, log)",
      ylabel = "QTcF (ms, log)"
    )
  )

  snapshot_plot(p, "eda-quantiles-log-axes")
})

test_that("eda_mean_dv_over_time SE error bars snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    reference_dose = "0 mg",
    error_bars = "SE"
  )

  snapshot_plot(p, "eda-mean-dv-time-se")
})

test_that("eda_mean_dv_over_time SE error bars dQTcF snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    error_bars = "SE"
  )

  snapshot_plot(p, "eda-mean-dv-time-se-dqtcf")
})

test_that("eda_mean_dv_over_time SD error bars dQTcF snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    error_bars = "SD"
  )

  snapshot_plot(p, "eda-mean-dv-time-sd")
})

test_that("eda_mean_dv_over_time SD error bars ddQTcF snapshot", {

  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil, deltaQTCF, NTLD, DOSEF,
    group_col = TRTG,
    reference_dose = "0 mg",
    error_bars = "SD"
  )

  snapshot_plot(p, "eda-mean-dv-time-sd-ddqtcf")
})

test_that("eda_hysteresis_loop_plot snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0) |>
    droplevels()

  p <- eda_hysteresis_loop_plot(
    data_proc, NTLD, deltaQTCF, CONC, DOSEF
  )

  snapshot_plot(p, "eda-hysteresis-loop")
})

test_that("model_results_spec validates and defaults", {
  s <- model_results_spec()
  expect_s3_class(s, "cqtkit_model_results_spec")
  expect_equal(unclass(s), list(slope = TRUE, ci = 0.90, pvalue = FALSE, eps = 0.001, digits = 3))
  expect_error(model_results_spec(ci = 2))
  expect_error(model_results_spec(digits = -1))
  expect_error(model_results_spec(pvalue = "yes"))
})

test_that("format_model_results honours the spec", {
  fmt <- function(...) format_model_results("LM", 0.12345, 0.1, 0.15, 0.0423, model_results_spec(...))
  expect_equal(fmt(), "LM Slope [90% CI]: 0.123 [0.100, 0.150]")
  expect_equal(fmt(pvalue = TRUE), "LM Slope [90% CI]: 0.123 [0.100, 0.150]\nSlope p-value: 0.042")
  expect_equal(fmt(pvalue = TRUE, eps = 0.05), "LM Slope [90% CI]: 0.123 [0.100, 0.150]\nSlope p-value: < 0.05")
  expect_equal(fmt(slope = FALSE, pvalue = TRUE), "Slope p-value: 0.042")
  expect_equal(fmt(ci = 0.95, digits = 2), "LM Slope [95% CI]: 0.12 [0.10, 0.15]")
  expect_equal(fmt(digits = 0), "LM Slope [90% CI]: 0 [0, 0]")
  expect_null(fmt(slope = FALSE))
})

test_that("eda_qt_rr_plot show_model_results accepts TRUE, FALSE, and a spec", {
  base <- eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lm")
  expect_match(base$labels$caption, "^Linear Regression Slope \\[90% CI\\]")
  expect_no_match(base$labels$caption, "p-value")

  as_true <- eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lm", show_model_results = TRUE)
  expect_identical(as_true$labels$caption, base$labels$caption)

  none <- eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lm", show_model_results = FALSE)
  expect_null(none$labels$caption)

  with_p <- eda_qt_rr_plot(
    cqtkit_data_verapamil, RR, QT, ID, model_type = "lm",
    show_model_results = model_results_spec(pvalue = TRUE, ci = 0.95)
  )
  expect_match(with_p$labels$caption, "\\[95% CI\\]")
  expect_match(with_p$labels$caption, "\nSlope p-value: ")

  expect_error(
    eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, show_model_results = "yes"),
    "model_results_spec"
  )
})

test_that("eda_qtc_comparison_plot passes the spec through", {
  p <- eda_qtc_comparison_plot(
    cqtkit_data_verapamil, RR, QT, QTCB, QTCF, ID, model_type = "lm",
    show_model_results = model_results_spec(pvalue = TRUE)
  )
  expect_match(p$labels$caption, "Slope p-value")
})

test_that("eda_mean_dv_over_time keeps dose factor level order in the legend", {
  lvls <- c("2.4 mg", "7.2 mg", "10 mg")
  dat <- cqtkit_data_verapamil |>
    dplyr::mutate(DOSEF = factor(lvls[(as.integer(factor(ID)) %% 3) + 1], levels = lvls))

  p <- eda_mean_dv_over_time(
    dat, deltaQTCF, NTLD, DOSEF,
    reference_threshold = c(-10, 10)
  )

  expect_s3_class(p$data$grouping, "factor")
  expect_equal(levels(p$data$grouping), lvls)

  legend <- function(p) ggplot2::get_guide_data(p, "colour")$.label
  expect_equal(legend(p), c(lvls, "Reference -10", "Reference 10"))

  # secondary DV: both series keep dose order, references trail
  p2 <- eda_mean_dv_over_time(
    dat, deltaQTCF, NTLD, DOSEF,
    secondary_data_col = CONC, reference_threshold = c(-10, 10)
  )
  expect_equal(
    legend(p2),
    c(paste(lvls, "deltaQTCF"), paste(lvls, "CONC"), "Reference -10", "Reference 10")
  )
})
