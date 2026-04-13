test_that("compute_lme_slope_df returns correct slope and CIs", {
  data_proc <- preprocess(cqtkit_data_verapamil)

  lme_mod <- fit_qtc_linear_model(
    data_proc, QT, RR, ID, method = "REML", remove_rr_iiv = FALSE
  )

  result <- compute_lme_slope_df(lme_mod, RR, conf_int = 0.9)

  # verify slope against fixef directly
  expect_equal(result$slope, unname(nlme::fixef(lme_mod)["RR"]))

  # verify CIs against intervals directly
  ci <- nlme::intervals(lme_mod, level = 0.9)$fixed
  expect_equal(result$slope_ci_lower, ci["RR", "lower"], tolerance = 1e-10)
  expect_equal(result$slope_ci_upper, ci["RR", "upper"], tolerance = 1e-10)
})
