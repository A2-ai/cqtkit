test_that("compute_fit_results residuals are observed minus predicted", {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    "REML",
    remove_conc_iiv = TRUE
  )

  res <- compute_fit_results(cqtkit_data_verapamil, mod, deltaQTCF, CONC, NTLD)

  expect_equal(res$RES, res$dv - res$PRED, ignore_attr = TRUE)
  expect_equal(res$IRES, res$dv - res$IPRED, ignore_attr = TRUE)
  expect_equal(unique(res$TRTG), "")
})
