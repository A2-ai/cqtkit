test_that("compute_grouped_mean_sd errors when time grouping does not reduce size", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::mutate('TIME_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(.test_data, QT, TIME_UNIQUE, DOSE),
    "ntime_col does not reduce size"
  )
})

test_that("compute_grouped_mean_sd errors when dose grouping does not reduce size", {
  .test_data <- cqtkit_data_verapamil |>
    dplyr::mutate('DOSE_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(.test_data, QT, NTLD, DOSE_UNIQUE),
    "dosef_col does not reduce size"
  )
})

test_that('compute_grouped_mean_sd computes correct mean, SD, SE, and CI', {
  result <- compute_grouped_mean_sd(cqtkit_data_verapamil, deltaQTCF, NTLD, DOSE, conf_int = 0.9)

  # manually compute for DOSE==120, NTLD==1
  subset <- cqtkit_data_verapamil |> dplyr::filter(DOSE == 120, NTLD == 1)
  vals <- subset$deltaQTCF

  row <- result |> dplyr::filter(dose == 120, time == 1)

  expect_equal(row$mean_dv, mean(vals), tolerance = 1e-10)
  expect_equal(row$sd, sd(vals), tolerance = 1e-10)
  expect_equal(row$n, length(vals))
  expect_equal(row$se, sd(vals) / sqrt(length(vals)), tolerance = 1e-10)

  expected_ci_low <- mean(vals) - qt((1 + 0.9) / 2, df = length(vals) - 1) * sd(vals) / sqrt(length(vals))
  expected_ci_high <- mean(vals) + qt((1 + 0.9) / 2, df = length(vals) - 1) * sd(vals) / sqrt(length(vals))
  expect_equal(row$ci_low, expected_ci_low, tolerance = 1e-10)
  expect_equal(row$ci_high, expected_ci_high, tolerance = 1e-10)
})

test_that('compute_grouped_mean_sd computes correct delta values with reference dose', {
  result <- compute_grouped_mean_sd(cqtkit_data_verapamil, deltaQTCF, NTLD, DOSE, reference_dose = 0, conf_int = 0.9)

  # at NTLD==1: mean_delta = mean(dose120) - mean(dose0)
  trt <- cqtkit_data_verapamil |> dplyr::filter(DOSE == 120, NTLD == 1)
  ref <- cqtkit_data_verapamil |> dplyr::filter(DOSE == 0, NTLD == 1)

  row <- result |> dplyr::filter(dose == 120, time == 1)

  expected_delta <- mean(trt$deltaQTCF) - mean(ref$deltaQTCF)
  expect_equal(row$mean_delta_dv, expected_delta, tolerance = 1e-10)

  # delta SE = sqrt(sd_trt^2/n_trt + sd_ref^2/n_ref)
  expected_delta_se <- sqrt(sd(trt$deltaQTCF)^2 / length(trt$deltaQTCF) +
    sd(ref$deltaQTCF)^2 / length(ref$deltaQTCF))
  expect_equal(row$delta_se, expected_delta_se, tolerance = 1e-10)

  # reference dose row should have delta of 0
  ref_row <- result |> dplyr::filter(dose == 0, time == 1)
  expect_equal(ref_row$mean_delta_dv, 0)
})
