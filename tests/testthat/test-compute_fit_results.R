test_that("compute_fit_results works", {
  data_proc <- cqtkit_data_verapamil %>% preprocess()
  mod <- fit_prespecified_model(
    data_proc,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    "REML",
    remove_conc_iiv = TRUE
  )

  expect_no_condition(compute_fit_results(
    data_proc,
    mod,
    deltaQTCF,
    CONC,
    NTLD,
    TRTG
  ))
})
