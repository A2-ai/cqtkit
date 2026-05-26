fit <- fit_prespecified_model(
  cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
  method = "REML", remove_conc_iiv = TRUE
)

test_that("compute_conc_for_upper_pred returns correct concentration with treatment group", {
  result <- compute_conc_for_upper_pred(
    cqtkit_data_verapamil, fit, "CONC", "TRTG", "Verapamil HCL",
    threshold = 10, conf_int = 0.9
  )

  # result should be a positive number
  expect_true(is.numeric(result))
  expect_true(result > 0)

  # verify independently using the quadratic formula
  df <- stats::coef(summary(fit))[1, 3]
  t_val <- stats::qt(1 - (1 - 0.9) / 2, df)
  v <- as.data.frame(stats::vcov(fit))

  theta_1 <- fit$coefficients$fixed[["CONC"]]
  var_theta_1 <- v["CONC", "CONC"]
  theta_3 <- fit$coefficients$fixed[["TRTGVerapamil HCL"]]
  var_theta_3 <- v["TRTGVerapamil HCL", "TRTGVerapamil HCL"]
  cov_13 <- v["TRTGVerapamil HCL", "CONC"]

  a <- theta_1^2 - var_theta_1 * t_val^2
  b <- 2 * theta_1 * (theta_3 - 10) - 2 * cov_13 * t_val^2
  cc <- (10 - theta_3)^2 - t_val^2 * var_theta_3

  disc <- b^2 - 4 * a * cc
  x0 <- (-b - sqrt(disc)) / (2 * a)
  x1 <- (-b + sqrt(disc)) / (2 * a)
  expected <- min(c(x0, x1)[c(x0, x1) > 0])

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("compute_conc_for_upper_pred works without treatment group", {
  # fit a simpler model without TRT
  fit_simple <- fit_prespecified_model(
    cqtkit_data_verapamil, deltaQTCF, ID, CONC, deltaQTCFBL,
    method = "REML", remove_conc_iiv = TRUE
  )

  result <- compute_conc_for_upper_pred(
    cqtkit_data_verapamil, fit_simple, "CONC",
    threshold = 10, conf_int = 0.9
  )

  # verify: conc = threshold / (theta_1 + t * sqrt(var_theta_1))
  df <- stats::coef(summary(fit_simple))[1, 3]
  t_val <- stats::qt(1 - (1 - 0.9) / 2, df)
  theta_1 <- fit_simple$coefficients$fixed[["CONC"]]
  var_theta_1 <- as.data.frame(stats::vcov(fit_simple))["CONC", "CONC"]

  expected <- 10 / (theta_1 + t_val * sqrt(var_theta_1))

  expect_equal(result, expected, tolerance = 1e-10)
})
