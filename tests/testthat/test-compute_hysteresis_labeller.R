test_that("hysteresis_labeller gives correct return type.", {
  .test_data <- cqtkit_data_verapamil %>%
    preprocess()

  expect_type(
    compute_hysteresis_labeller(
      .test_data,
      NTLD,
      deltaQTCF,
      CONC,
      DOSEF
    ),
    "closure"
  )
})

test_that("hysteresis_labeller errors with non-factor DOSE", {
  expect_error(
    compute_hysteresis_labeller(
      cqtkit_data_verapamil %>% preprocess(),
      NTLD,
      deltaQTCF,
      CONC,
      DOSE
    )
  )
})

test_that("hysteresis_labeller errors when NTLD is supplied as factor", {
  .test_data <- cqtkit_data_verapamil %>%
    preprocess() %>%
    dplyr::mutate(NTLDF = as.factor(NTLD))

  expect_error(
    compute_hysteresis_labeller(
      .test_data,
      NTLDF,
      deltaQTCF,
      CONC,
      DOSE
    )
  )
})
