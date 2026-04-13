test_that('compute_lm_fit_df does not run for vector inputs, rather than column names', {
  data <- cqtkit_data_verapamil |> preprocess()

  expect_error(
    compute_lm_fit_df(
      data,
      data$RR,
      QT
    )
  )
})

#This doesn't fail because of quoted vs unquoted and rlang::enquo, but rather formula is messed up...
test_that('compute_lm_fit_df does not run string column names', {
  data <- cqtkit_data_verapamil |> preprocess()

  expect_error(
    compute_lm_fit_df(
      data,
      'RR',
      'QT'
    )
  )
})

test_that('compute_lm_fit_df works with non standard evaluation', {
  data <- cqtkit_data_verapamil |> preprocess()

  expect_no_condition(
    compute_lm_fit_df(
      data,
      RR,
      QT
    )
  )
})

test_that('compute_lm_fit_df returns correct coefficients and CIs', {
  data <- cqtkit_data_verapamil |> preprocess()

  result <- compute_lm_fit_df(data, RR, QT, conf_int = 0.9)

  # independent lm
  ref_model <- lm(QT ~ RR, data = data)
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
