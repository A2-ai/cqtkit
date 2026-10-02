test_that("tabulate_study_summary snapshot", {
  table <- tabulate_study_summary(
    cqtkit_data_verapamil,
    TRTG,
    ID,
    protocol_number = "A2AI201",
    title = "C-QT Analysis Study",
    study_status = "Completed"
  )

  snapshot_gt(table, "tab-study-summary")
})

test_that("tabulate_pk_parameters snapshot", {
  data_proc <- cqtkit_data_verapamil |> dplyr::filter(DOSE != 0)

  table <- tabulate_pk_parameters(data_proc, ID, DOSE, CONC, NTLD)

  snapshot_gt(table, "tab-pk-params")
})

test_that("tabulate_model_fit_parameters snapshot", {
  fit <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    "REML",
    TRUE
  )

  table <- tabulate_model_fit_parameters(fit, "TRTG", "TAFD", "ID")

  snapshot_gt(table, "tab-model-fit-params")
})

test_that("tabulate_ecg_param_summary snapshot", {
  table <- tabulate_ecg_param_summary(
    cqtkit_data_verapamil,
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF,
    "QTcF",
    "ms",
    reference_dose = "0 mg"
  )

  snapshot_gt(table, "tab-ecg-param-summary")
})

test_that("tabulate_high_qtc_observations snapshot", {
  table <- tabulate_high_qtc_observations(
    cqtkit_data_verapamil,
    QTCF,
    deltaQTCF,
    qtc_label = "QTcF"
  )

  snapshot_gt(table, "tab-high-qtc-observations")
})

test_that("tabulate_high_qtc_subjects snapshot", {
  table <- tabulate_high_qtc_subjects(
    cqtkit_data_verapamil,
    QTCF,
    deltaQTCF,
    id_col = ID,
    group_col = TRTG,
    qtc_label = "QTcF"
  )

  snapshot_gt(table, "tab-high-qtc-subjects")
})

test_that("tabulate_exposure_predictions snapshot", {
  fit <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    "REML",
    TRUE
  )

  pk_df <- compute_pk_parameters(
    cqtkit_data_verapamil |> dplyr::filter(DOSE != 0),
    ID,
    DOSEF,
    CONC,
    NTLD
  )

  table <- tabulate_exposure_predictions(
    cqtkit_data_verapamil,
    fit,
    CONC,
    list(CONC = 10, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "2 HR"),
    list(CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "2 HR"),
    doses = c(120),
    cmaxes = c(pk_df[[1, "Cmax_gm"]])
  )

  snapshot_gt(table, "tab-exposure-pred")
})

test_that("tabulate_ecg_param_summary footnotes cells left empty", {
  single <- cqtkit_data_verapamil |>
    dplyr::filter(DOSEF == "120 mg") |>
    dplyr::slice(1) |>
    dplyr::mutate(NTLD = 99)
  .test_data <- cqtkit_data_verapamil |>
    dplyr::filter(!(NTLD == 2 & DOSEF == "0 mg")) |>
    dplyr::bind_rows(single)

  messages <- character()
  table <- withCallingHandlers(
    tabulate_ecg_param_summary(
      .test_data,
      NTLD,
      DOSEF,
      QTCF,
      deltaQTCF,
      "QTcF",
      "ms",
      reference_dose = "0 mg"
    ),
    warning = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(
    messages,
    c(
      "Fewer than 2 observations at: 99 / 120 mg. Their CIs are NA.",
      "No `reference_dose` (0 mg) observations at: 2, 99. Differences from `reference_dose` are NA there."
    )
  )

  notes <- table$`_footnotes`
  notes$time <- table$`_data`$time[notes$rownum]
  notes$text <- unlist(notes$footnotes)

  reference_notes <- notes[notes$text == "No 0 mg observations at this time point.", ]
  expect_setequal(unique(reference_notes$time), c(2, 99))
  expect_setequal(unique(reference_notes$colname), c("mean_ddecg", "ddecg_low"))

  single_notes <- notes[notes$text == "Fewer than 2 observations; CI not computed.", ]
  expect_setequal(unique(single_notes$time), 99)
  expect_setequal(single_notes$colname, c("ecg_low", "decg_low", "ddecg_low"))
})

test_that("tabulate_ecg_param_summary footnote_missing = FALSE adds no footnotes", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::filter(!(NTLD == 2 & DOSEF == "0 mg"))

  table <- suppressWarnings(tabulate_ecg_param_summary(
    .test_data,
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF,
    "QTcF",
    "ms",
    reference_dose = "0 mg",
    footnote_missing = FALSE
  ))

  expect_equal(nrow(table$`_footnotes`), 0)
})

test_that("tabulate_ecg_param_summary adds no footnotes or warnings on complete data", {
  expect_no_warning(
    table <- tabulate_ecg_param_summary(
      cqtkit_data_verapamil,
      NTLD,
      DOSEF,
      QTCF,
      deltaQTCF,
      "QTcF",
      "ms",
      reference_dose = "0 mg",
      delta_ecg_param_conf_int = 0.9
    )
  )
  expect_equal(nrow(table$`_footnotes`), 0)
})
