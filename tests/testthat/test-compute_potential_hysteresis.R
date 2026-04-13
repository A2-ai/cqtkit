test_that('compute_potential_hysteresis dose not work for multi-dose inputs', {
  .test_data <- dplyr::filter(cqtkit_data_verapamil |> preprocess())
  expect_error(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEF),
  )
})

test_that('compute_potential_hysteresis dose not work for only two time points', {
  .test_data <- dplyr::filter(
    cqtkit_data_verapamil |> preprocess(),
    DOSE == 250,
    NTLD %in% c(0.5, 1)
  )
  expect_error(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEF),
  )
})

test_that('compute_potential_hysteresis warns about multiple times with max CONC', {
  .test_data <- tibble::tibble(
    NTLD = c(0.1, 0.5, 1, 4),
    deltaQTCF = c(0, 4, 3, 1),
    CONC = c(0, 10, 10, 2),
    DOSEN = c(33, 33, 33, 33)
  )
  expect_warning(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEN)
  )
})

test_that('compute_potential_hysteresis returns FALSE when no lag between Tmax and Umax', {
  # Tmax == Umax (max conc and max dQTCF at same time) -> no hysteresis
  .test_data <- tibble::tibble(
    NTLD = c(0.5, 1, 2, 3, 4, 5),
    deltaQTCF = c(2, 10, 6, 4, 3, 1),
    CONC = c(5, 50, 30, 15, 8, 2),
    DOSE = c(100, 100, 100, 100, 100, 100)
  )
  result <- compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSE)
  expect_false(result)
})

test_that('compute_potential_hysteresis returns TRUE when effect lags concentration', {
  # Tmax at 1h, Umax at 3h (2h lag), many timepoints > 5ms
  .test_data <- tibble::tibble(
    NTLD = c(0.5, 1, 2, 3, 4, 5),
    deltaQTCF = c(2, 6, 8, 10, 7, 6),
    CONC = c(20, 50, 30, 15, 8, 2),
    DOSE = c(100, 100, 100, 100, 100, 100)
  )
  result <- compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSE)
  expect_true(result)
})

test_that('compute_potential_hysteresis returns FALSE for verapamil (known no hysteresis)', {
  .test_data <- cqtkit_data_verapamil |>
    preprocess() |>
    dplyr::filter(DOSE == 120)
  result <- compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEF)
  expect_false(result)
})

test_that('compute_potential_hysteresis warns about multiple times with max dQTCF', {
  .test_data <- tibble::tibble(
    NTLD = c(0.1, 0.5, 1, 4),
    deltaQTCF = c(0, 4, 4, 1),
    CONC = c(0, 10, 4, 2),
    DOSEN = c(33, 33, 33, 33)
  )
  expect_warning(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEN)
  )
})
