test_that("hysteresis_labeller labels each dose with its hysteresis result", {
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::filter(DOSE > 0) %>%
    droplevels()
  looped <- tibble::tibble(
    NTLD = 1:6,
    deltaQTCF = c(6, 7, 8, 12, 9, 2),
    CONC = c(10, 30, 20, 15, 10, 5),
    DOSEF = factor("10 mg")
  )

  none <- compute_hysteresis_labeller(.test_data, NTLD, deltaQTCF, CONC, DOSEF)
  found <- compute_hysteresis_labeller(looped, NTLD, deltaQTCF, CONC, DOSEF)

  expect_equal(none(), list(`120 mg` = "120 mg"))
  expect_equal(found(), list(`10 mg` = "10 mg : Hysteresis detected"))
})

test_that("hysteresis_labeller errors with non-factor DOSE", {
  expect_error(
    compute_hysteresis_labeller(
      cqtkit_data_verapamil,
      NTLD,
      deltaQTCF,
      CONC,
      DOSE
    ),
    "Must be of type 'factor'"
  )
})

test_that("hysteresis_labeller errors when NTLD is supplied as factor", {
  .test_data <- cqtkit_data_verapamil %>%
    dplyr::filter(DOSE > 0) %>%
    droplevels() %>%
    dplyr::mutate(NTLDF = as.factor(NTLD))

  expect_error(
    compute_hysteresis_labeller(
      .test_data,
      NTLDF,
      deltaQTCF,
      CONC,
      DOSEF
    ),
    "!is.factor(qtc_conc_df$ntld)",
    fixed = TRUE
  )
})
