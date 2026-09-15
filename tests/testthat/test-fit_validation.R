model_data <- cqtkit_data_verapamil %>% preprocess()

test_that("non-syntactic model column names error before fitting", {
  d <- model_data
  d[["Dose Group"]] <- d$TRTG

  expect_error(
    fit_prespecified_model(
      d,
      deltaQTCF,
      ID,
      CONC,
      deltaQTCFBL,
      `Dose Group`,
      TAFD
    ),
    "must be syntactic"
  )
})

test_that("syntactic column names are not flagged", {
  expect_silent(assert_syntactic_names(c("CONC", "deltaQTCF", "TRTG.1")))
})

test_that("a factor collapsing below two levels errors and names the cause", {
  d <- model_data
  d$CONC[d$TRTG == "Placebo"] <- NA

  expect_error(
    fit_prespecified_model(d, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD),
    'Column "TRTG" collapses to a single level'
  )
  expect_error(
    fit_prespecified_model(d, deltaQTCF, ID, CONC, deltaQTCFBL, TRTG, TAFD),
    "always NA in column\\(s\\): CONC"
  )
})

test_that("a factor losing a level warns but still fits", {
  d <- model_data
  d$CONC[d$TAFD == "24 HR"] <- NA

  expect_warning(
    fit <- fit_prespecified_model(
      d,
      deltaQTCF,
      ID,
      CONC,
      deltaQTCFBL,
      TRTG,
      TAFD,
      remove_conc_iiv = TRUE
    ),
    'Column "TAFD" lost level\\(s\\).*"24 HR"'
  )
  expect_s3_class(fit, "lme")
})

test_that("complete data fits without warning", {
  expect_silent(
    fit_prespecified_model(
      model_data,
      deltaQTCF,
      ID,
      CONC,
      deltaQTCFBL,
      TRTG,
      TAFD,
      remove_conc_iiv = TRUE
    )
  )
})
