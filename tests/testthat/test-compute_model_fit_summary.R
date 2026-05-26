test_that("compute_model_fit_parameters errors when fit is not lme class", {
  expect_error(compute_model_fit_parameters(cqtkit_data_verapamil), "lme")
})

test_that("compute_model_fit_parameters includes reference levels when requested", {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )

  without_ref <- compute_model_fit_parameters(mod, include_reference_levels = FALSE)
  with_ref <- compute_model_fit_parameters(mod, include_reference_levels = TRUE)

  # with reference levels should have more rows
  expect_gt(nrow(with_ref), nrow(without_ref))

  # reference rows should have "(Reference)" in name and Value = 0
  ref_rows <- with_ref |> dplyr::filter(grepl("Reference", Parameters))
  expect_true(nrow(ref_rows) > 0)
  expect_true(all(ref_rows$Value == 0))
  expect_true(all(is.na(ref_rows$Std.Error)))
})

test_that('compute_model_fit_parameters errors for conf_int > 1', {
  mod <- fit_prespecified_model(
    cqtkit_data_verapamil,
    deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD,
    remove_conc_iiv = TRUE
  )

  expect_error(compute_model_fit_parameters(mod, conf_int = 95), "conf_int")
})
