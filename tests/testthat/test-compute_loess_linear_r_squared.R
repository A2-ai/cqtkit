test_that("compute_loess_linear_r_squared compares the loess and linear fits of deltaQTc on concentration", {
  withr::local_options(lifecycle_verbosity = "quiet")
  lin <- stats::lm(deltaQTCF ~ CONC, data = cqtkit_data_verapamil)
  lo <- stats::loess(deltaQTCF ~ CONC, data = cqtkit_data_verapamil, span = 0.99)
  expected <- 1 -
    sum((stats::predict(lin) - stats::predict(lo))^2) /
      sum((stats::predict(lin) - mean(stats::predict(lin)))^2)

  res <- compute_loess_linear_r_squared(cqtkit_data_verapamil, deltaQTCF, CONC)

  expect_equal(res$R_squared, expected)
})

test_that("compute_loess_linear_r_squared is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    compute_loess_linear_r_squared(cqtkit_data_verapamil, deltaQTCF, CONC),
    "`compute_loess_linear_r_squared\\(\\)` was deprecated in cqtkit 1.2.1"
  )
})
