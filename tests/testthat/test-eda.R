test_that("eda_qt_rr_plot default snapshot", {

  p <- eda_qt_rr_plot(cqtkit_data_verapamil, RR, QT, ID, model_type = "lm")

  snapshot_plot(p, "eda-qt-rr-default")
})

test_that("eda_qt_rr_plot with style snapshot", {

  p <- eda_qt_rr_plot(
    cqtkit_data_verapamil, RR, QT, ID, TRTG,
    model_type = "lme",
    style = set_style(
      title = "QT-RR Relationship",
      xlabel = "RR Interval (ms)",
      ylabel = "QT Interval (ms)",
      legend = "Treatment",
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
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
    style = set_style(
      xlabel = "Concentration (ng/mL)",
      ylabel = "deltaQTcF (ms)",
      ylims = c(-40, 40),
      legend = "Treatment",
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
    style = set_style(
      ylabel = bquote("Mean " ~ Delta ~ Delta ~ "QTcF (ms)"),
      xlabel = "Time (hr)",
      legend = "Treatment",
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
    style = set_style(
      title = "Concentration-QTc Relationship",
      xlabel = "Concentration (ng/mL)",
      ylabel = "deltaQTcF (ms)",
      colors = c("Placebo" = "grey", "Verapamil HCL" = "steelblue"),
      shapes = c("Verapamil HCL" = 17),
      legend_nrow = 1,
      legend.title.hjust = "center",
      caption_hjust = "right",
      color_order = 1,
      shape_order = 1,
      linetype_order = 2
    )
  )

  snapshot_plot(p, "eda-scatter-styled")
})

test_that("eda_quantiles_plot with log axes snapshot", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0)

  p <- eda_quantiles_plot(
    data_proc, CONC, QTCF, trt_col = TRTG,
    style = set_style(
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
