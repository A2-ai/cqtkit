test_that('compute_lm_fit_df errors for invalid column inputs', {

  # vector input instead of column name
  expect_error(compute_lm_fit_df(cqtkit_data_verapamil, cqtkit_data_verapamil$RR, QT))

  # string input instead of unquoted name
  expect_error(compute_lm_fit_df(cqtkit_data_verapamil, 'RR', 'QT'))
})

test_that('compute_lm_fit_df returns correct coefficients and CIs', {

  result <- compute_lm_fit_df(cqtkit_data_verapamil, RR, QT, conf_int = 0.9)

  # independent lm
  ref_model <- lm(QT ~ RR, data = cqtkit_data_verapamil)
  ref_ci <- confint(ref_model, level = 0.9)

  expect_equal(result$intercept, unname(coef(ref_model)[1]))
  expect_equal(result$slope, unname(coef(ref_model)[2]))
  expect_equal(result$intercept_ci_lower, ref_ci["(Intercept)", 1])
  expect_equal(result$intercept_ci_upper, ref_ci["(Intercept)", 2])
  expect_equal(result$slope_ci_lower, ref_ci["RR", 1])
  expect_equal(result$slope_ci_upper, ref_ci["RR", 2])
  expect_equal(result$p_value_slope, summary(ref_model)$coefficients["RR", "Pr(>|t|)"])
  expect_equal(result$conf_int, 0.9)
})
