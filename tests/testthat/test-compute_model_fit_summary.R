test_that("compute_model_fit_parameters errors when fit is not lme class", {
  expect_error(compute_model_fit_parameters(data))
})

test_that('compute_model_fit_paramters works for different conf_int', {
  mod <- fit_prespecified_model(
    data %>% preprocess(),
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )

  expect_no_condition(compute_model_fit_parameters(mod, conf_int = 0.5))
})

test_that('compute_model_fit_pararmeter errors for conf_int> 1', {
  mod <- fit_prespecified_model(
    data %>% preprocess(),
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )

  expect_error(compute_model_fit_parameters(mod, conf_int = 95))
})
