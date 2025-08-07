test_that("fit_prespecified_model throws an error if required columns are missing", {
  incomplete_data <- data %>% dplyr::select(-CONC) #Remove an essential column

  expect_error(
    fit_prespecified_model(
      incomplete_data,
      deltaQTCF,
      ID,
      CONC,
      deltaQTCFBL,
      TRTG,
      TAFD
    ),
    "must.include"
  )
})

test_that("fit_prespecified_model correctly orders TAFD factor levels in the model", {
  model_data <- data %>% preprocess()
  mod <- fit_prespecified_model(
    model_data,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )

  # Extract the data used in the model via model.frame()
  model_frame <- model.frame(mod)

  # Retrieve the TAFD column from the model's data
  tafd_in_model <- model_frame[["TAFD"]]

  expect_true(
    is.factor(tafd_in_model),
    info = "TAFD should be a factor in the model"
  )
  expect_equal(
    levels(tafd_in_model),
    unique(as.character(tafd_in_model)),
    info = "The factor levels of TAFD should be in the order they first appear"
  )
})


test_that("fit_prespecified_model correctly removes inter-individual variability (IIV) when specified", {
  model_data <- data %>% preprocess()

  mod_with_iiv <- fit_prespecified_model(
    model_data,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = FALSE
  )
  mod_without_iiv <- fit_prespecified_model(
    model_data,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    remove_conc_iiv = TRUE
  )

  expect_true(all(
    as.character(mod_with_iiv$call$random) == c("~", "1 + CONC | ID")
  ))
  expect_true(all(
    as.character(mod_without_iiv$call$random) == c("~", "1 | ID")
  ))
})

test_that("fit_prespecified_model correctly applies ML and REML methods", {
  model_data <- data %>% preprocess()

  mod_ml <- fit_prespecified_model(
    model_data,
    deltaQTCF,
    ID,
    CONC,
    deltaQTCFBL,
    TRTG,
    TAFD,
    method = "ML",
    remove_conc_iiv = TRUE
  )
  mod_reml <- fit_prespecified_model(
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

  expect_false(identical(nlme::fixef(mod_ml), nlme::fixef(mod_reml))) # Fixed effects should differ
})
