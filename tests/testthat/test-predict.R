fit <- fit_prespecified_model(
  cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
  method = "REML", remove_conc_iiv = TRUE
)

trt_pred <- list(
  CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "2 HR"
)
ctrl_pred <- list(
  CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "2 HR"
)

pk_df <- compute_pk_parameters(
  cqtkit_data_verapamil |> dplyr::filter(DOSE != 0), ID, DOSEF, CONC, NTLD
)

test_that("predict_with_observations_plot dQTcF snapshot", {
  trt_pred_single <- list(
    CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "0.5 HR"
  )

  p <- predict_with_observations_plot(
    cqtkit_data_verapamil, fit, CONC, deltaQTCF,
    treatment_predictors = trt_pred_single,
    reference_threshold = 10
  )

  snapshot_plot(p, "predict-observations-dqtcf")
})

test_that("predict_with_observations_plot ddQTcF snapshot", {
  p <- predict_with_observations_plot(
    cqtkit_data_verapamil, fit, CONC, deltaQTCF,
    treatment_predictors = trt_pred,
    control_predictors = ctrl_pred,
    id_col = ID, ntime_col = NTLD, trt_col = TRTG,
    reference_threshold = c(-10, 10),
    style = ggstylekit::style_spec(
      ylabel = bquote(Delta ~ Delta ~ "QTcF (ms)"),
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "predict-observations-ddqtcf")
})

test_that("predict_with_quantiles_plot dQTcF snapshot", {
  trt_pred_single <- list(
    CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "0.5 HR"
  )

  p <- predict_with_quantiles_plot(
    cqtkit_data_verapamil, fit, CONC, deltaQTCF,
    treatment_predictors = trt_pred_single,
    reference_threshold = 10
  )

  snapshot_plot(p, "predict-quantiles")
})

test_that("predict_with_quantiles_plot ddQTcF with reference time snapshot", {
  trt_pred_ref <- list(
    CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "0.5 HR"
  )
  ctrl_pred_ref <- list(
    CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "0.5 HR"
  )

  p <- predict_with_quantiles_plot(
    cqtkit_data_verapamil, fit, CONC, deltaQTCF,
    treatment_predictors = trt_pred_ref,
    control_predictors = ctrl_pred_ref,
    id_col = ID, ntime_col = NTLD, trt_col = TRTG,
    reference_threshold = 10,
    style = ggstylekit::style_spec(
      ylabel = bquote(Delta ~ Delta ~ "QTcF (ms)"),
      xlabel = "Concentration (ng/mL)"
    )
  )

  snapshot_plot(p, "predict-quantiles-ddqtcf")
})

test_that("predict_with_exposure_plot dQTcF snapshot", {
  trt_pred_single <- list(
    CONC = 0, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "0.5 HR"
  )

  p <- predict_with_exposure_plot(
    cqtkit_data_verapamil, fit, CONC,
    treatment_predictors = trt_pred_single,
    cmaxes = pk_df[[1, "Cmax_gm"]],
    reference_threshold = 10
  )

  snapshot_plot(p, "predict-exposure-dqtcf")
})

test_that("predict_with_exposure_plot ddQTcF snapshot", {
  p <- predict_with_exposure_plot(
    cqtkit_data_verapamil, fit, CONC,
    treatment_predictors = trt_pred,
    control_predictors = ctrl_pred,
    cmaxes = pk_df[[1, "Cmax_gm"]],
    reference_threshold = c(-10, 10),
    style = ggstylekit::style_spec(
      ylabel = bquote(Delta ~ Delta ~ "QTcF (ms)"),
      legend.position = "bottom"
    )
  )

  snapshot_plot(p, "predict-exposure-ddqtcf")
})
