test_that('compute_lm_fit_df does not run for vector inputs, rather than column names', {
  data <- data %>% preprocess()

  expect_error(
    compute_lm_fit_df(
      data,
      data$RR,
      QT
    )
  )
})

#This doesn't fail because of quoted vs unquoted and rlang::enquo, but rather formula is messed up...
test_that('compute_lm_fit_df does not run string column names', {
  data <- data %>% preprocess()

  expect_error(
    compute_lm_fit_df(
      data,
      'RR',
      'QT'
    )
  )
})

test_that('compute_lm_fit_df works with non standard evaluation', {
  data <- data %>% preprocess()

  expect_no_condition(
    compute_lm_fit_df(
      data,
      RR,
      QT
    )
  )
})
