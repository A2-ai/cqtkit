test_that('compute_potential_hysteresis dose not work for multi-dose inputs', {
  .test_data <- dplyr::filter(data %>% preprocess())
  expect_error(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEF),
  )
})

test_that('compute_potential_hysteresis dose not work for only two time points', {
  .test_data <- dplyr::filter(
    data %>% preprocess(),
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
