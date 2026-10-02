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
theta <- nlme::fixef(fit)
v <- stats::vcov(fit)
t_90 <- stats::qt(0.95, stats::coef(summary(fit))[1, 3])

test_that("the upper bound of the slope-only prediction crosses the threshold at the returned concentration", {
  conc <- compute_conc_for_upper_pred(cqtkit_data_verapamil, fit, "CONC")

  upper <- conc * (theta[["CONC"]] + t_90 * sqrt(v["CONC", "CONC"]))
  expect_equal(upper, 10)
})

test_that("the upper bound of the treatment prediction crosses the threshold at the returned concentration", {
  conc <- compute_conc_for_upper_pred(
    cqtkit_data_verapamil,
    fit,
    "CONC",
    "TRTG",
    "Verapamil HCL"
  )

  trt <- "TRTGVerapamil HCL"
  upper <- theta[["CONC"]] * conc +
    theta[[trt]] +
    t_90 *
      sqrt(
        v["CONC", "CONC"] * conc^2 +
          v[trt, trt] +
          2 * conc * v["CONC", trt]
      )
  expect_equal(upper, 10)
})
