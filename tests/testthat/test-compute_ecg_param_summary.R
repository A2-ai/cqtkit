test_that("compute_ecg_param_summary returns correct means without reference dose", {
  data <- cqtkit_data_verapamil |> preprocess()
  result <- compute_ecg_param_summary(data, NTLD, DOSE, QTCF, deltaQTCF)

  # verify ecg mean for DOSE==120, time==1 against manual calculation
  subset <- data |> dplyr::filter(DOSE == 120, NTLD == 1)
  row <- result |> dplyr::filter(dose == 120, time == 1)

  expect_equal(row$mean_ecg, mean(subset$QTCF), tolerance = 1e-10)
  expect_equal(row$mean_decg, mean(subset$deltaQTCF), tolerance = 1e-10)

  # should not have ddecg columns without reference_dose
  expect_false("mean_ddecg" %in% names(result))
})

test_that("compute_ecg_param_summary returns correct delta-delta with reference dose", {
  data <- cqtkit_data_verapamil |> preprocess()
  result <- compute_ecg_param_summary(
    data, NTLD, DOSE, QTCF, deltaQTCF, reference_dose = 0
  )

  # should have ddecg columns
  expect_true("mean_ddecg" %in% names(result))

  # at reference dose, ddecg should be 0
  ref_row <- result |> dplyr::filter(dose == 0, time == 1)
  expect_equal(ref_row$mean_ddecg, 0)

  # ddecg for treatment = mean_decg(trt) - mean_decg(ref) at same time
  trt_row <- result |> dplyr::filter(dose == 120, time == 1)
  expected_dd <- trt_row$mean_decg - ref_row$mean_decg
  expect_equal(trt_row$mean_ddecg, expected_dd, tolerance = 1e-10)
})
