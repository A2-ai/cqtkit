test_that('compute_deltas works when all columns present', {
  .test_data <- cqtkit_data_verapamil %>% compute_qtcb_qtcf()
  expect_no_condition(
    compute_deltas(.test_data)
  )
})

test_that('compute_deltas warns for missing columns', {
  data <- cqtkit_data_verapamil %>%
    dplyr::select(-QTCB, -QTCF)
  expect_error(compute_deltas(data))
})
