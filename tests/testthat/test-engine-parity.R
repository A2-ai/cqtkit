# Engine parity: the same default call, rendered by the list engine and by the
# spec engine, compared against one baseline.
#
# Only defaults are compared. A caller who passes a style list keeps the list
# engine and the figure it has always produced; a caller who writes a
# style_spec() has opted into the new engine. What has to hold is that an
# untouched call draws the same figure either way.
#
# The baselines in _snaps/engine-parity/ are copies of the list-path snapshots,
# so a .new.svg here means the ggstylekit path draws that figure differently
# from the figure cqtkit produces today. Review them with
# testthat::snapshot_review("engine-parity").
#
# Snapshot names match the list-path names on purpose: vdiffr draws the name
# into the image, so an equal name is the only way the comparison isolates the
# engine.

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

trt_pred_single <- list(
  CONC = 0,
  deltaQTCFBL = 0,
  TRTG = "Verapamil HCL",
  TAFD = "0.5 HR"
)

pk_df <- compute_pk_parameters(
  cqtkit_data_verapamil |> dplyr::filter(DOSE != 0),
  ID,
  DOSEF,
  CONC,
  NTLD
)

test_that("eda_qt_rr_plot parity", {
  p <- eda_qt_rr_plot(
    cqtkit_data_verapamil,
    RR,
    QT,
    ID,
    model_type = "lm",
    style = style_spec()
  )

  snapshot_plot(p, "eda-qt-rr-default")
})

test_that("eda_qtc_comparison_plot parity", {
  p <- eda_qtc_comparison_plot(
    cqtkit_data_verapamil,
    RR,
    QT,
    QTCB,
    QTCF,
    id_col = ID,
    trt_col = TRTG,
    model_type = "lme",
    show_model_results = TRUE,
    remove_rr_iiv = TRUE,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "eda-qtc-comparison")
})

test_that("eda_quantiles_plot parity", {
  data_proc <- cqtkit_data_verapamil |> dplyr::filter(DOSE > 0)

  p <- eda_quantiles_plot(
    data_proc,
    CONC,
    deltaQTCF,
    trt_col = TRTG,
    style = style_spec()
  )

  snapshot_plot(p, "eda-quantiles-default")
})

test_that("eda_scatter_with_regressions parity", {
  p <- eda_scatter_with_regressions(
    cqtkit_data_verapamil,
    deltaQTCF,
    CONC,
    TRTG,
    reference_threshold = 10,
    style = style_spec()
  )

  snapshot_plot(p, "eda-scatter-regressions-default")
})

test_that("eda_hysteresis_loop_plot parity", {
  data_proc <- cqtkit_data_verapamil |>
    dplyr::filter(DOSE > 0) |>
    droplevels()

  p <- eda_hysteresis_loop_plot(
    data_proc,
    NTLD,
    deltaQTCF,
    CONC,
    DOSEF,
    style = style_spec()
  )

  snapshot_plot(p, "eda-hysteresis-loop")
})

test_that("eda_mean_dv_over_time parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    reference_threshold = 10,
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-default")
})

test_that("gof_plots parity", {
  p <- gof_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-plots")
})

test_that("gof_concordance_plots parity", {
  p <- gof_concordance_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-concordance")
})

test_that("gof_residuals_plots parity", {
  p <- gof_residuals_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-residuals")
})

test_that("gof_qq_plots parity", {
  p <- gof_qq_plots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-qq")
})

test_that("gof_residuals_time_boxplots parity", {
  p <- gof_residuals_time_boxplots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-residuals-time-box")
})

test_that("gof_residuals_trt_boxplots parity", {
  p <- gof_residuals_trt_boxplots(
    cqtkit_data_verapamil,
    fit,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG,
    style = style_spec()
  )

  expect_s3_class(p, "patchwork")
  snapshot_plot(p, "gof-residuals-trt-box")
})

test_that("gof_vpc_plot parity", {
  p <- suppressWarnings(gof_vpc_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    deltaQTCF,
    nruns = 10,
    seed = 804831,
    style = style_spec()
  ))

  snapshot_plot(p, "gof-vpc")
})

test_that("predict_with_observations_plot parity", {
  p <- predict_with_observations_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    deltaQTCF,
    treatment_predictors = trt_pred_single,
    reference_threshold = 10,
    style = style_spec()
  )

  snapshot_plot(p, "predict-observations-dqtcf")
})

test_that("predict_with_quantiles_plot parity", {
  expect_warning(
    p <- predict_with_quantiles_plot(
      cqtkit_data_verapamil,
      fit,
      CONC,
      deltaQTCF,
      treatment_predictors = trt_pred_single,
      reference_threshold = 10,
      style = style_spec()
    ),
    "quantiles had duplicates"
  )

  snapshot_plot(p, "predict-quantiles")
})

test_that("predict_with_exposure_plot parity", {
  p <- predict_with_exposure_plot(
    cqtkit_data_verapamil,
    fit,
    CONC,
    treatment_predictors = trt_pred_single,
    cmaxes = pk_df[[1, "Cmax_gm"]],
    reference_threshold = 10,
    style = style_spec()
  )

  snapshot_plot(p, "predict-exposure-dqtcf")
})

test_that("eda_qt_rr_plot p-value parity", {
  p <- eda_qt_rr_plot(
    cqtkit_data_verapamil,
    RR,
    QT,
    ID,
    model_type = "lm",
    include_pvalue = TRUE,
    style = style_spec()
  )

  snapshot_plot(p, "eda-qt-rr-pvalue")
})

test_that("eda_mean_dv_over_time SE parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    reference_dose = "0 mg",
    error_bars = "SE",
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-se")
})

test_that("eda_mean_dv_over_time SE dQTcF parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    error_bars = "SE",
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-se-dqtcf")
})

test_that("eda_mean_dv_over_time SD parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    error_bars = "SD",
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-sd")
})

test_that("eda_mean_dv_over_time SD ddQTcF parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    group_col = TRTG,
    reference_dose = "0 mg",
    error_bars = "SD",
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-sd-ddqtcf")
})

test_that("eda_mean_dv_over_time PK overlay parity", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    secondary_data_col = CONC,
    group_col = TRTG,
    reference_dose = "0 mg",
    reference_threshold = c(-10, 10),
    style = style_spec()
  )

  snapshot_plot(p, "eda-mean-dv-time-pk-ddqtcf")
})
