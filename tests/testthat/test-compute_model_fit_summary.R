test_that("compute_model_fit_parameters errors when fit is not lme class", {
  expect_error(
    compute_model_fit_parameters(cqtkit_data_verapamil),
    "Must inherit from class 'lme'"
  )
})

test_that('compute_model_fit_pararmeter errors for conf_int> 1', {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil %>% preprocess(),
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )

  expect_error(
    compute_model_fit_parameters(mod, conf_int = 95),
    "Element 1 is not <= 1"
  )
})

test_that("default model summaries do not require the original model data", {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil |> preprocess(),
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )
  mod$data <- NULL
  mod$call$data <- quote(model_data_that_is_no_longer_available)

  expect_no_condition(compute_model_fit_parameters(mod))
})
