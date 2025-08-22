test_that('compute_exposure_predictions runs fine for good model and full data', {
  data_proc <- preprocess(cqtkit_data_verapamil)
  mod <- fit_prespecified_model(
    data_proc,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    method = "ML",
    remove_conc_iiv = FALSE
  )

  expect_no_error(
    compute_exposure_predictions(
      data_proc,
      mod,
      CONC,
      treatment_predictors = list(
        CONC = 10,
        deltaQTCFBL = 0,
        TRTG = "Verapamil HCL",
        TAFD = "1 HR"
      ),
      control_predictors = list(
        CONC = 0,
        deltaQTCFBL = 0,
        TRTG = "Placebo",
        TAFD = "1 HR"
      )
    )
  )
})


test_that('compute_exposure_predictions does not add non-unique conc values with cmaxes', {
  data_proc <- preprocess(cqtkit_data_dofetilide)
  mod <- fit_prespecified_model(
    data_proc,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    method = "ML",
    remove_conc_iiv = FALSE
  )

  expo_pred <- compute_exposure_predictions(
    data_proc,
    mod,
    CONC,
    treatment_predictors = list(
      CONC = 10,
      deltaQTCFBL = 0,
      TRTG = "Dofetilide",
      TAFD = "1 HR"
    ),
    control_predictors = list(
      CONC = 0,
      deltaQTCFBL = 0,
      TRTG = "Placebo",
      TAFD = "1 HR"
    ),
    cmaxes = c(100, 200, 300)
  )

  expect_true(
    expo_pred$conc |> length() == expo_pred$conc |> unique() |> length()
  )
})
