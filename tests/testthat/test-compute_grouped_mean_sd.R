test_that("compute_grouped_mean_sd errors when time grouping does not reduce size", {
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::mutate('TIME_UNIQUE' = dplyr::row_number())
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
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::mutate('DOSE_UNIQUE' = dplyr::row_number())
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
    cqtkit_data_verapamil %>% preprocess(),
    deltaQTCF,
    NTLD,
    DOSE
  )
  expect_true('mean_dv' %in% colnames(result))
})

test_that('compute_grouped_mean_sd gives mean_delta_dv when reference dose given.', {
  result <- compute_grouped_mean_sd(
    cqtkit_data_verapamil %>% preprocess(),
    deltaQTCF,
    NTLD,
    DOSE,
    reference_dose = 0
  )
  expect_true('mean_delta_dv' %in% colnames(result))
})

test_that("grouped summaries return ordered factor groups", {
  data <- cqtkit_data_verapamil |> preprocess()

  plain <- compute_grouped_mean_sd(data, deltaQTCF, NTLD, DOSEF)
  grouped <- compute_grouped_mean_sd(data, deltaQTCF, NTLD, DOSEF, TRTG)
  plain_ref <- compute_grouped_mean_sd(
    data,
    deltaQTCF,
    NTLD,
    DOSEF,
    reference_dose = "0 mg"
  )
  grouped_ref <- compute_grouped_mean_sd(
    data,
    deltaQTCF,
    NTLD,
    DOSEF,
    TRTG,
    reference_dose = "0 mg"
  )

  expect_s3_class(plain$group, "factor")
  expect_equal(levels(plain$group), c("0 mg", "120 mg"))
  expect_equal(dplyr::group_vars(plain), c("time", "dose"))

  expected_group_levels <- c("0 mg Placebo", "120 mg Verapamil HCL")
  expect_s3_class(grouped$group, "factor")
  expect_equal(levels(grouped$group), expected_group_levels)
  expect_equal(dplyr::group_vars(grouped), c("time", "dose", "group"))

  expect_s3_class(plain_ref$group, "factor")
  expect_s3_class(grouped_ref$group, "factor")
  expect_equal(dplyr::group_vars(plain_ref), "time")
  expect_equal(dplyr::group_vars(grouped_ref), "time")

  plain_keys <- as.data.frame(plain[c("time", "dose", "group")])
  plain_ref_keys <- as.data.frame(plain_ref[c("time", "dose", "group")])
  grouped_keys <- as.data.frame(grouped[c("time", "dose", "group")])
  grouped_ref_keys <- as.data.frame(grouped_ref[c("time", "dose", "group")])
  expect_identical(plain_keys, plain_ref_keys)
  expect_identical(grouped_keys, grouped_ref_keys)
})

test_that("ECG summaries retain factor group levels", {
  result <- compute_ecg_param_summary(
    cqtkit_data_verapamil |> preprocess(),
    NTLD,
    DOSEF,
    QTCF,
    deltaQTCF
  )

  expect_s3_class(result$group, "factor")
  expect_equal(levels(result$group), c("0 mg", "120 mg"))
})
