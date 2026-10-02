test_that("eda_mean_dv_over_time orders a secondary series after the primary", {
  p <- eda_mean_dv_over_time(
    cqtkit_data_verapamil,
    deltaQTCF,
    NTLD,
    DOSEF,
    secondary_data_col = CONC
  )

  expect_equal(
    levels(p$data$grouping),
    c("0 mg deltaQTCF", "120 mg deltaQTCF", "0 mg CONC", "120 mg CONC")
  )
})
