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

  expect_warning(
    res <- compute_fit_results(cqtkit_data_verapamil, mod, deltaQTCF, CONC, NTLD),
    "`TRTG` in `data` is overwritten"
  )

  expect_equal(res$RES, res$dv - res$PRED, ignore_attr = TRUE)
  expect_equal(res$IRES, res$dv - res$IPRED, ignore_attr = TRUE)
  expect_equal(unique(res$TRTG), "")
  expect_identical(
    names(res)[1:10],
    c("dv", "conc", "time", "PRED", "IPRED", "RES", "IRES", "WRES", "IWRES", "TRTG")
  )
})

test_that("compute_fit_results reads quoted column names as columns", {
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

  plain <- compute_fit_results(cqtkit_data_verapamil, mod, deltaQTCF, CONC, NTLD, TRTG)
  quoted <- compute_fit_results(cqtkit_data_verapamil, mod, "deltaQTCF", "CONC", "NTLD", "TRTG")
  expect_identical(quoted, plain)
})
