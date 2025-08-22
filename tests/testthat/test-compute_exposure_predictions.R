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
