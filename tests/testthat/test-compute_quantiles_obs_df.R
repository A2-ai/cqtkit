test_that("compute_quantiles_obs_df messages when NA are in CONC data", {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE > 0)
  .test_data[[1, 'CONC']] <- NA
  expect_warning(
    compute_quantiles_obs_df(
      .test_data,
      CONC,
      deltaQTCF
    )
  )
})

test_that("compute_quantiles_obs_df returns correct number of bins and statistics", {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE > 0)

  result <- compute_quantiles_obs_df(.test_data, CONC, deltaQTCF, nbins = 5)

  # should have 5 bins
  expect_equal(nrow(result), 5)

  # each bin's mean should equal independent calculation
  # reconstruct the bins to verify one of them
  breaks <- quantile(.test_data$CONC, probs = seq(0, 1, length.out = 6), na.rm = TRUE, type = 2)
  bins <- cut(.test_data$CONC, breaks = unique(breaks), include.lowest = TRUE)

  bin_data <- split(.test_data$deltaQTCF, bins)
  first_bin_vals <- bin_data[[1]]

  expect_equal(result$mean_dv[1], mean(first_bin_vals), tolerance = 1e-6)
  expect_equal(result$sd[1], sd(first_bin_vals), tolerance = 1e-6)
  expect_equal(result$n[1], length(first_bin_vals))
})

test_that("compute_quantiles_obs_df messages when NA are in deltaQTC data", {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE > 0)
  .test_data[[1, 'deltaQTCF']] <- NA
  expect_warning(
    compute_quantiles_obs_df(
      .test_data,
      CONC,
      deltaQTCF
    )
  )
})
