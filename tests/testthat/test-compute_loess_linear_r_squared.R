test_that('compute_loess_linear_r_squared partial determination matches independent calc', {
  data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE > 0)

  result <- compute_loess_linear_r_squared(data, deltaQTCF, CONC, span = 0.99)

  lin_reg <- lm(deltaQTCF ~ CONC, data = data)
  loess_reg <- loess(deltaQTCF ~ CONC, data = data, span = 0.99)

  expected_partial_det <- 1 - sum(loess_reg$residuals^2) / sum(lin_reg$residuals^2)
  expect_equal(result$R_partial_det, expected_partial_det, tolerance = 1e-10)
})
