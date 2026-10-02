test_that("eda_hysteresis_loop_plot plots ddQTc against the reference dose", {
  p <- eda_hysteresis_loop_plot(
    cqtkit_data_verapamil,
    NTLD,
    deltaQTCF,
    CONC,
    DOSEF,
    reference_dose = "0 mg"
  )
  ref <- compute_grouped_mean_sd(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )

  expect_equal(p$data$meandQTC, ref$mean_delta_dv[ref$dose == "120 mg"])
})

test_that("eda_hysteresis_loop_plot errors at time points with no reference dose", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::filter(!(NTLD %in% c(2, 24) & DOSEF == "0 mg"))

  expect_error(
    suppressWarnings(eda_hysteresis_loop_plot(
      .test_data,
      NTLD,
      deltaQTCF,
      CONC,
      DOSEF,
      reference_dose = "0 mg"
    )),
    "Mean `deltaqtc_col` is NA at time points 2, 24 for group \"120 mg\"",
    fixed = TRUE
  )
})
