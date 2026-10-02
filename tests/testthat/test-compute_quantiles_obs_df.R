test_that("compute_quantiles_obs_df warns when NA are in CONC data", {
  .test_data <- cqtkit_data_verapamil %>% dplyr::filter(DOSE > 0)
  .test_data[[1, 'CONC']] <- NA
  expect_warning(
    compute_quantiles_obs_df(.test_data, CONC, deltaQTCF),
    "Your X data contains NA"
  )
})

test_that("compute_quantiles_obs_df warns when NA are in deltaQTC data", {
  .test_data <- cqtkit_data_verapamil %>% dplyr::filter(DOSE > 0)
  .test_data[[1, 'deltaQTCF']] <- NA
  expect_warning(
    compute_quantiles_obs_df(.test_data, CONC, deltaQTCF),
    "Your Y data contains NA"
  )
})
