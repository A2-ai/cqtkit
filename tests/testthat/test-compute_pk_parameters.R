test_that("compute_pk_parameters warns when NA are present in data", {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE != 0)
  .test_data$CONC[[1]] <- NA

  expect_warning(
    compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})

test_that('compute_pk_paramters errors for 0 in CONC data', {
  .test_data <- cqtkit_data_verapamil |> preprocess()
  .test_data$CONC <- ifelse(.test_data$DOSE == 0, 0, .test_data$CONC)

  expect_error(
    compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})

test_that('compute_pk_parameters works for no 0 and no NA', {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE != 0)

  expect_no_warning(
    #i Actually got a <dplyr_regroup> with text: -- how to remove these?
    result <- compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})

test_that('compute_pk_parameters computes correct geometric mean with unequal timepoints', {
  # quinidine has 12-15 rows per subject-dose, so subjects with more
  # timepoints would be overweighted without deduplication
  .test_data <- cqtkit_data_quinidine |>
    preprocess() |>
    dplyr::filter(DOSE != 0)

  result <- compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)

  # correct Cmax geometric mean: one Cmax per subject, then exp(mean(log(.)))
  cmax_per_subj <- .test_data |>
    dplyr::group_by(ID, DOSE) |>
    dplyr::summarize(Cmax = max(CONC), .groups = "drop")

  expected_gm <- exp(mean(log(cmax_per_subj$Cmax)))
  expected_cv <- sqrt(exp(stats::sd(log(cmax_per_subj$Cmax))^2) - 1) * 100

  expect_equal(result$Cmax_gm, expected_gm, tolerance = 1e-6)
  expect_equal(result$Cmax_cv, expected_cv, tolerance = 1e-6)
})
