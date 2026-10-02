test_that("compute_lme_slope_df returns the slope row of the parameter table", {
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

  slope <- compute_lme_slope_df(fit, CONC, conf_int = 0.9)
  params <- compute_model_fit_parameters(fit, conf_int = 0.9)
  conc_row <- params[params$Parameters == "CONC", ]

  expect_equal(slope$slope, conc_row$Value)
  expect_equal(slope$slope_ci_lower, conc_row$CIl)
  expect_equal(slope$slope_ci_upper, conc_row$CIu)
})
