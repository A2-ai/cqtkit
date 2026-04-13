test_that('compute_exposure_predictions returns predictions over concentration range', {
  data_proc <- preprocess(cqtkit_data_verapamil)
  mod <- fit_prespecified_model(
    data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    method = "ML", remove_conc_iiv = FALSE
  )

  result <- compute_exposure_predictions(
    data_proc, mod, CONC,
    treatment_predictors = list(
      CONC = 10, deltaQTCFBL = 0, TRTG = "Verapamil HCL", TAFD = "1 HR"
    ),
    control_predictors = list(
      CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "1 HR"
    )
  )

  # output should have expected columns
  expect_true(all(c("conc", "pred", "lower", "upper") %in% names(result)))

  # predictions should be monotonically ordered by concentration
  expect_true(all(diff(result$conc) > 0))

  # CI should bracket the prediction
  expect_true(all(result$lower <= result$pred))
  expect_true(all(result$upper >= result$pred))

  # at conc = 0, ddQTcF should be the treatment intercept difference (small)
  zero_row <- result |> dplyr::filter(conc == 0)
  expect_true(abs(zero_row$pred) < 10)
})

test_that('compute_exposure_predictions does not add non-unique conc values with cmaxes', {
  data_proc <- preprocess(cqtkit_data_dofetilide)
  mod <- fit_prespecified_model(
    data_proc, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    method = "ML", remove_conc_iiv = TRUE
  )

  expo_pred <- compute_exposure_predictions(
    data_proc, mod, CONC,
    treatment_predictors = list(
      CONC = 10, deltaQTCFBL = 0, TRTG = "Dofetilide", TAFD = "1 HR"
    ),
    control_predictors = list(
      CONC = 0, deltaQTCFBL = 0, TRTG = "Placebo", TAFD = "1 HR"
    ),
    cmaxes = c(100, 200, 300)
  )

  expect_true(
    expo_pred$conc |> length() == expo_pred$conc |> unique() |> length()
  )
})
