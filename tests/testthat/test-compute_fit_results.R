test_that("compute_fit_results returns correct predictions and residuals", {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    method = "REML", remove_conc_iiv = TRUE
  )

  result <- compute_fit_results(cqtkit_data_verapamil, mod, deltaQTCF, CONC, NTLD, TRTG)

  # verify expected columns exist
  expected_cols <- c("dv", "conc", "time", "TRTG", "PRED", "IPRED", "RES", "IRES", "WRES", "IWRES")
  expect_true(all(expected_cols %in% names(result)))

  # PRED should match population-level predict
  expect_equal(as.numeric(result$PRED), as.numeric(stats::predict(mod, level = 0)), tolerance = 1e-10)

  # RES = dv - PRED
  expect_equal(as.numeric(result$RES), as.numeric(result$dv - result$PRED), tolerance = 1e-10)

  # IRES = dv - IPRED
  expect_equal(as.numeric(result$IRES), as.numeric(result$dv - result$IPRED), tolerance = 1e-10)
})
