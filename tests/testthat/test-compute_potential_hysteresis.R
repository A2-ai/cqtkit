test_that("compute_potential_hysteresis does not work for multi-dose inputs", {
  expect_error(
    compute_potential_hysteresis(cqtkit_data_verapamil, NTLD, deltaQTCF, CONC, DOSEF),
    '`group_col` must contain a single group; found "0 mg", "120 mg".',
    fixed = TRUE
  )
})

test_that("compute_potential_hysteresis needs at least 4 time points", {
  .test_data <- dplyr::filter(
    cqtkit_data_verapamil,
    DOSE == 120,
    NTLD %in% c(0.5, 1, 2)
  )
  expect_error(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEF),
    '`ntime_col` needs at least 4 time points per group to assess hysteresis; "120 mg" has 3.',
    fixed = TRUE
  )
})

test_that("compute_potential_hysteresis detects a lagged, sustained effect", {
  looped <- tibble::tibble(
    NTLD = 1:6,
    deltaQTCF = c(6, 7, 8, 12, 9, 2),
    CONC = c(10, 30, 20, 15, 10, 5),
    DOSEN = 33
  )

  expect_true(
    compute_potential_hysteresis(looped, NTLD, deltaQTCF, CONC, DOSEN)
  )
})

test_that("compute_potential_hysteresis warns about multiple times with max CONC", {
  .test_data <- tibble::tibble(
    NTLD = c(0.1, 0.5, 1, 4),
    deltaQTCF = c(0, 4, 3, 1),
    CONC = c(0, 10, 10, 2),
    DOSEN = c(33, 33, 33, 33)
  )
  expect_warning(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEN),
    "Multiple times have max CONC"
  )
})

test_that("compute_potential_hysteresis warns about multiple times with max dQTCF", {
  .test_data <- tibble::tibble(
    NTLD = c(0.1, 0.5, 1, 4),
    deltaQTCF = c(0, 4, 4, 1),
    CONC = c(0, 10, 4, 2),
    DOSEN = c(33, 33, 33, 33)
  )
  expect_warning(
    compute_potential_hysteresis(.test_data, NTLD, deltaQTCF, CONC, DOSEN),
    "Multiple times have max dQTCF"
  )
})
