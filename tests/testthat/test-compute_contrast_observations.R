trt_pred <- list(TRTG = "Verapamil HCL")
ctrl_pred <- list(TRTG = "Placebo")

test_that("group contrasts subtract the control mean at each time", {
  obs <- compute_contrast_observations(
    cqtkit_data_verapamil,
    CONC,
    deltaQTCF,
    ntime_col = NTLD,
    trt_col = TRTG,
    treatment_predictors = trt_pred,
    control_predictors = ctrl_pred,
    contrast_method = "group"
  )
  control_means <- cqtkit_data_verapamil |>
    dplyr::filter(TRTG == "Placebo") |>
    dplyr::group_by(NTLD) |>
    dplyr::summarise(control = mean(deltaQTCF))
  expected <- cqtkit_data_verapamil |>
    dplyr::filter(TRTG == "Verapamil HCL") |>
    dplyr::left_join(control_means, by = "NTLD")

  expect_equal(obs$dv, expected$deltaQTCF - expected$control)
})

test_that("group contrasts name the missing columns", {
  expect_error(
    compute_contrast_observations(
      cqtkit_data_verapamil,
      CONC,
      deltaQTCF,
      treatment_predictors = trt_pred,
      control_predictors = ctrl_pred,
      contrast_method = "group"
    ),
    "you must also supply: trt_col, ntime_col"
  )
})

test_that("group contrasts drop and warn about times with no control mean", {
  d <- cqtkit_data_verapamil
  d$deltaQTCF[d$TRTG == "Placebo" & d$NTLD == 1] <- NA

  expect_warning(
    obs <- compute_contrast_observations(
      d,
      CONC,
      deltaQTCF,
      ntime_col = NTLD,
      trt_col = TRTG,
      treatment_predictors = trt_pred,
      control_predictors = ctrl_pred,
      contrast_method = "group"
    ),
    "Control group means contained NA"
  )
  expect_false(1 %in% obs$NTLD)
})
