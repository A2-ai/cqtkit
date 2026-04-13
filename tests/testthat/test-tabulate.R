test_that("tabulate_study_summary snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  table <- tabulate_study_summary(
    data_proc, TRTG, ID,
    protocol_number = "A2AI201",
    title = "C-QT Analysis Study",
    study_status = "Completed"
  )

  snapshot_gt(table, "tab-study-summary")
})

test_that("tabulate_pk_parameters snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil) |>
    dplyr::filter(DOSE != 0)

  table <- tabulate_pk_parameters(data_proc, ID, DOSE, CONC, NTLD)

  snapshot_gt(table, "tab-pk-params")
})

test_that("tabulate_model_fit_parameters snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  fit <- fit_prespecified_model(
    data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD, "REML", TRUE
  )

  table <- tabulate_model_fit_parameters(fit, "TRTG", "TAFD", "ID")

  snapshot_gt(table, "tab-model-fit-params")
})

test_that("tabulate_ecg_param_summary snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  table <- tabulate_ecg_param_summary(
    data_proc, NTLD, DOSEF, QTCF, deltaQTCF,
    "QTcF", "ms",
    reference_dose = "0 mg"
  )

  snapshot_gt(table, "tab-ecg-param-summary")
})

test_that("tabulate_high_qtc_obs snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  table <- tabulate_high_qtc_obs(data_proc, QTCF, deltaQTCF, qtc_label = "QTcF")

  snapshot_gt(table, "tab-high-qtc-obs")
})

test_that("tabulate_high_qtc_sub snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  table <- tabulate_high_qtc_sub(data_proc, QTCF, deltaQTCF, ID, qtc_label = "QTcF")

  snapshot_gt(table, "tab-high-qtc-sub")
})

test_that("tabulate_exposure_predictions snapshot", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  fit <- fit_prespecified_model(
    data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD, "REML", TRUE
  )

  pk_df <- compute_pk_parameters(
    data_proc |> dplyr::filter(DOSE != 0), ID, DOSEF, CONC, NTLD
  )

  table <- tabulate_exposure_predictions(
    data_proc, fit, CONC,
    list(CONC = 10, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "2 HR"),
    list(CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "2 HR"),
    doses = c(120),
    cmaxes = c(pk_df[[1, "Cmax_gm"]])
  )

  snapshot_gt(table, "tab-exposure-pred")
})
