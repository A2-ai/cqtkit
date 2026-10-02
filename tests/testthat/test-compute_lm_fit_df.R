test_that("compute_lm_fit_df does not run for vector inputs, rather than column names", {
  expect_error(
    compute_lm_fit_df(cqtkit_data_verapamil, cqtkit_data_verapamil$RR, QT),
    "Names must include"
  )
})
