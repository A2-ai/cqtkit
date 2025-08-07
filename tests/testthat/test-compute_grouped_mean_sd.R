test_that("compute_grouped_mean_sd errors when time grouping does not reduce size", {
  .test_data <- data %>% dplyr::mutate('TIME_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(
      .test_data,
      QT,
      TIME_UNIQUE,
      DOSE
    )
  )
})

test_that("compute_grouped_mean_sd errors when dose grouping does not reduce size", {
  .test_data <- data %>% dplyr::mutate('DOSE_UNIQUE' = dplyr::row_number())
  expect_error(
    compute_grouped_mean_sd(
      .test_data,
      QT,
      NTLD,
      DOSE_UNIQUE
    )
  )
})

test_that('compute_grouped_mean_sd gives mean_dv column', {
  result <- compute_grouped_mean_sd(
    data %>% preprocess(),
    deltaQTCF,
    NTLD,
    DOSE
  )
  expect_true('mean_dv' %in% colnames(result))
})

test_that('compute_grouped_mean_sd gives mean_delta_dv when reference dose given.', {
  result <- compute_grouped_mean_sd(
    data %>% preprocess(),
    deltaQTCF,
    NTLD,
    DOSE,
    reference_dose = 0
  )
  expect_true('mean_delta_dv' %in% colnames(result))
})
