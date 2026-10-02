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

test_that("compute_deltas subtracts each baseline", {
  data <- cqtkit_data_verapamil |>
    dplyr::select(
      -deltaQTCB,
      -deltaQTCF,
      -deltaRR,
      -deltaHR,
      -deltaQT
    )

  out <- compute_deltas(data)

  expect_equal(out$deltaQTCB, data$QTCB - data$QTCBBL)
  expect_equal(out$deltaQTCF, data$QTCF - data$QTCFBL)
  expect_equal(out$deltaRR, data$RR - data$RRBL)
  expect_equal(out$deltaHR, data$HR - data$HRBL)
  expect_equal(out$deltaQT, data$QT - data$QTBL)
})
