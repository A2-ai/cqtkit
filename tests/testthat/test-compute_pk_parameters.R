test_that("compute_pk_parameters warns when NA are present in data", {
  .test_data <- data %>% preprocess() %>% dplyr::filter(DOSE != 0)
  .test_data$CONC[[1]] <- NA

  expect_warning(
    compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})

test_that('compute_pk_paramters errors for 0 in CONC data', {
  .test_data <- data %>% preprocess()
  .test_data$CONC <- ifelse(.test_data$DOSE == 0, 0, .test_data$CONC)

  expect_error(
    compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})

test_that('compute_pk_parameters works for no 0 and no NA', {
  .test_data <- data %>% preprocess() %>% dplyr::filter(DOSE != 0)

  expect_no_warning(
    #i Actually got a <dplyr_regroup> with text: -- how to remove these?
    result <- compute_pk_parameters(.test_data, ID, DOSE, CONC, NTLD)
  )
})
