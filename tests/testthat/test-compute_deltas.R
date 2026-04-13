test_that('compute_deltas computes correct delta values', {
  .test_data <- cqtkit_data_verapamil |> compute_qtcb_qtcf()
  result <- compute_deltas(.test_data)

  expect_equal(result$deltaQTCB, .test_data$QTCB - .test_data$QTCBBL, tolerance = 1e-10)
  expect_equal(result$deltaQTCF, .test_data$QTCF - .test_data$QTCFBL, tolerance = 1e-10)
  expect_equal(result$deltaRR, .test_data$RR - .test_data$RRBL, tolerance = 1e-10)
  expect_equal(result$deltaHR, .test_data$HR - .test_data$HRBL, tolerance = 1e-10)
  expect_equal(result$deltaQT, .test_data$QT - .test_data$QTBL, tolerance = 1e-10)
})

test_that('compute_deltas errors for missing columns', {
  data <- cqtkit_data_verapamil |>
    dplyr::select(-QTCB, -QTCF)
  expect_error(compute_deltas(data), "must include")
})
