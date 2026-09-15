model_data <- cqtkit_data_verapamil %>% preprocess()
fit <- fit_prespecified_model(
  model_data,
  deltaQTCF,
  ID,
  CONC,
  deltaQTCFBL,
  TRTG,
  TAFD,
  method = "REML",
  remove_conc_iiv = TRUE
)

test_that("seed makes simulation summaries reproducible", {
  sims <- function(seed) {
    suppressWarnings(compute_summary_statistics_of_simulations(
      model_data,
      fit,
      CONC,
      conf_int = 0.9,
      nruns = 5,
      nbins = 10,
      seed = seed
    ))
  }

  expect_identical(sims(42), sims(42))
  expect_false(identical(sims(42), sims(43)))
})

test_that("gof_vpc_plot passes seed through to the simulations", {
  # The simulated prediction intervals are the ribbon layer; the plot's own
  # $data is the observed quantiles and does not vary with the seed.
  ribbon <- function(seed) {
    suppressWarnings(gof_vpc_plot(
      model_data,
      fit,
      CONC,
      deltaQTCF,
      nruns = 5,
      seed = seed
    ))$layers[[2]]$data
  }

  expect_identical(ribbon(42), ribbon(42))
  expect_false(identical(ribbon(42), ribbon(43)))
})

test_that("seed does not disturb the caller's RNG stream", {
  set.seed(1)
  before <- .Random.seed

  invisible(suppressWarnings(gof_vpc_plot(
    model_data,
    fit,
    CONC,
    deltaQTCF,
    nruns = 2,
    seed = 99
  )))

  expect_identical(.Random.seed, before)
})

test_that("seed = NULL does not fix the simulations", {
  sims <- function() {
    suppressWarnings(compute_summary_statistics_of_simulations(
      model_data,
      fit,
      CONC,
      conf_int = 0.9,
      nruns = 5,
      nbins = 10
    ))
  }

  expect_false(identical(sims(), sims()))
})
